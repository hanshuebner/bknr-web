(in-package :bknr.mail)

(enable-interpol-syntax)

(define-persistent-class mail-handler ()
  ((email :update
	  :index-type string-unique-index
	  :index-reader mail-handler-with-mail
	  :index-values all-mail-handlers)))

(defgeneric handle-mail (mail-handler mail))

(defmethod handle-incoming-mail ((mail mail))
  (let ((mail-handler (mail-handler-with-mail (mail-to mail))))
    (when mail-handler
      (handle-mail mail-handler mail))))

(define-persistent-class subscription ()
  ((mailinglist :read)
   (user :read)
   (hash :read
	 :index-type string-unique-index
	 :index-reader subscription-with-hash)
   
   (type :update :documentation "(or :mail :digest :imap)")
   (started :read :initarg :started)
   (sent-mails :update :initform nil)))

(defmethod print-object ((subscription subscription) stream)
  (with-slots (user mailinglist type) subscription
    (format stream "#<~a ID: ~a USER: ~a ML: ~a TYPE: ~a>"
	    (class-name (class-of subscription))
	    (store-object-id subscription)
	    (user-login user)
	    (mailinglist-name mailinglist)
	    type))
  subscription)

(defmethod subscription-unsubscribe-url ((subscription subscription))
  (format nil "~a/unsubscribe/~a"
	  (mailinglist-website-url (subscription-mailinglist subscription))
	  (subscription-hash subscription)))

(defmethod subscription-bounce-address ((subscription subscription))
  (format nil "bounce-~a@~a"
	  (subscription-hash subscription)
	  (mailinglist-mail-domain (subscription-mailinglist subscription))))

(deftransaction subscription-has-received-mail (subscription mail)
  (push mail (subscription-sent-mails subscription)))

(defmethod delete-store-object :before ((subscription subscription))
  (with-slots (mailinglist user) subscription
    (setf (mailinglist-subscriptions mailinglist)
	  (remove subscription (mailinglist-subscriptions mailinglist)))
    (setf (user-subscriptions user)
	  (remove subscription (user-subscriptions user)))))

(define-persistent-class mailinglist (mail-handler)
  ((name :update
	 :index-type string-unique-index
	 :index-reader mailinglist-with-name
	 :index-values all-mailinglists)   
   (description :update :initform "")
   (subscriptions :update :initform nil)
   (website-url :update :initform nil)
   (archives :update :initform nil)
   (subscriber-only :update :initform t)
   (header :update :initform "")
   (footer :update :initform "")
   (mail-domain :update :initform "bknr.net")))

(defun make-mailinglist (name email &rest args)
  (apply #'make-instance 'mailinglist :name name :email email args))

;; backwards compatibility
(defmethod mailinglist-email ((mailinglist mailinglist))
  (mail-handler-email mailinglist))

(defmethod print-object ((ml mailinglist) stream)
  (format stream "#<~a ID: ~a NAME: ~a EMAIL: ~A>"
	  (class-name (class-of ml))
	  (store-object-id ml)
	  (mailinglist-name ml)
	  (mailinglist-email ml))
  ml)

(defmethod delete-store-object :before ((mailinglist mailinglist))
  (dolist (subscription (mailinglist-subscriptions mailinglist))
    (delete-store-object subscription)))

(defun mailinglist-with-email (email)
  (mail-handler-with-mail email))

(defmethod mailinglist-users ((mailinglist mailinglist))
  (mapcar #'subscription-user (mailinglist-subscriptions mailinglist)))

(defmethod mailinglist-user-with-email ((mailinglist mailinglist) email)
  (find email (mailinglist-users mailinglist)
	:key #'user-email
	:test #'string-equal))

(defun make-headers (&rest headers)
  (loop for (name value) on headers by #'cddr
     collect (format nil "~:(~a~): ~a" name value)))

;; send mail
(defmethod mailinglist-send-mail ((mailinglist mailinglist) mail &key resend)
  (mailinglist-archive-mail mailinglist mail)
  (dolist (subscription (mailinglist-subscriptions mailinglist))
    (handler-case
	(with-slots (user type) subscription
	  (when (eq type :mail)
	    (when (and (user-reachable-by-mail-p user)
		       (or resend
			   (not (find mail (subscription-sent-mails subscription)))))
	      (user-send-mail user mail
			      :sender (subscription-bounce-address subscription)
			      :headers (make-headers :errors-to (subscription-bounce-address subscription)
						     :subject (mail-subject mail)
						     :x-mailinglist-email (mailinglist-email mailinglist)
						     :x-mailinglist-name (mailinglist-name mailinglist)
						     :x-mailinglist-unsubscribe-url (subscription-unsubscribe-url subscription)))
	      (subscription-has-received-mail subscription mail))))
      (error (e)
	(warn "error ~a while sending ~a to ~a"
	      e mail subscription)))))

;; receive mail
(defmethod handle-mail ((mailinglist mailinglist) mail)
  (with-slots (to from subject body) mail
    (let ((user (mailinglist-user-with-email mailinglist (mail-from mail))))
      (if (mailinglist-subscriber-only mailinglist)
	  (when user
	    (mailinglist-send-mail mailinglist mail))
	  (mailinglist-send-mail mailinglist mail)))))

;; transactions
(defun mailinglist-archive-mail (mailinglist mail)
  (with-transaction ("mailinglist-archive-mail")
    (with-slots (archives) mailinglist
      (unless (find mail archives)
	(push archives mail)))))

;; subscription
(define-condition mailinglist-condition (error)
  ((user :initarg :user :reader mailinglist-condition-user)
   (mailinglist :initarg :mailinglist :reader mailinglist-condition-mailinglist)))

(define-condition already-subscribed-condition (mailinglist-condition)
  ())

(defmethod print-object ((condition already-subscribed-condition) stream)
  (format stream "#<~a: ~a already subscribed to ~a>"
	  (class-name (class-of condition))
	  (user-email (mailinglist-condition-user condition))
	  (mailinglist-name (mailinglist-condition-mailinglist condition)))
  condition)

(define-condition not-subscribed-condition (mailinglist-condition)
  ())

(defmethod print-object ((condition not-subscribed-condition) stream)
  (format stream "#<~a: ~a is not subscribed to ~a>"
	  (class-name (class-of condition))
	  (user-email (mailinglist-condition-user condition))
	  (mailinglist-name (mailinglist-condition-mailinglist condition)))
  condition)

(defvar *subscription-count* 0)

(defun mailinglist-subscribe-user (mailinglist user &key (type :mail) (started (get-universal-time)))
  (if (member user (mailinglist-users mailinglist))
      (error (make-condition 'already-subscribed-condition
			     :user user
			     :mailinglist mailinglist))
      (with-transaction ("mailinglist-subscribe-user")
	(let ((subscription (make-instance 'subscription
                                           :hash (make-capability-string)
                                           :mailinglist mailinglist
                                           :type type
                                           :user user
                                           :started started)))
	  (push subscription (mailinglist-subscriptions mailinglist))
	  (push subscription (user-subscriptions user))))))

(defun mailinglist-unsubscribe-user (mailinglist user)
  (let ((subscription (find user (mailinglist-subscriptions mailinglist)
			    :key #'subscription-user)))
    (if subscription
	(delete-object subscription))
    (error (make-condition 'not-subscribed-condition
			   :user user
			   :mailinglist mailinglist))))
