(in-package :bknr.mail)

(enable-interpol-syntax)

(define-persistent-class registration ()
  ((full-name :read :initform "")
   (password :read :initform nil)

   (hash :read
	 :index-type string-unique-index
	 :index-reader registration-with-hash
	 :index-values all-registrations)

   (login :read)
   (email :read)
   (timestamp :read :initform (get-universal-time))
   (subscribe-mailinglist :read)))

(defun registration-with-email (email)
  (find email (class-instances 'registration) :key #'registration-email :test #'string-equal))

(defun registration-with-login (login)
  (find login (class-instances 'registration) :key #'registration-login :test #'string-equal))

(defun make-registration (&rest args)
  (apply #'make-instance 'registration :hash (make-capability-string)
	 args))

(defmethod print-object ((registration registration) stream)
  (format stream "#<~a ID: ~a LOGIN: ~a>"
	  (class-name (class-of registration))
	  (store-object-id registration)
	  (registration-login registration))
  registration)

(defmethod confirm-registration ((registration registration))
  "Confirm a registration.  If the user does not exist, create new user
object.  Create subscription for the given mailing list."
  (with-slots (login full-name password hash email subscribe-mailinglist) registration
    (format t ";; processing registration request for login ~a, mailing list ~a~%" login subscribe-mailinglist)
    (handler-case
	(let ((user (or (find-user login)
			(make-instance 'user
                                       :login login
                                       :email email
                                       :full-name full-name
                                       :password password
                                       :flags '(:registered)))))
	  (when subscribe-mailinglist
	    (mailinglist-subscribe-user subscribe-mailinglist user)))
      (error (e)
	(warn "error ignored while processing registration: ~a" e)))
    (delete-object registration)))

(defmethod registration-confirm-url ((registration registration))
  (format nil "~a/confirm/~a"
	  (mailinglist-website-url (registration-subscribe-mailinglist registration))
	  (registration-hash registration)))

(define-persistent-class registration-handler (mail-handler)
  ((registrations :update :initform nil)))

(defmethod handle-mail ((handler registration-handler) mail)
  (let ((registration (registration-with-email (mail-from mail))))
    (when registration
      (with-slots (hash) registration
	(when (scan #?"\(hash ${hash}\)" (mail-subject mail))
          (confirm-registration registration))))))

(defmethod make-test-registrations ((mailinglist mailinglist) count email-format)
  (loop for i from 1 upto count
     for email = (format nil email-format i)
     do (confirm-registration (make-registration :login email :email email :subscribe-mailinglist mailinglist))))
