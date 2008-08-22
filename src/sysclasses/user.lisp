(in-package :bknr.user)

;;; XXX allow empty slots with NIL because make-instance sucks...

(defmethod initiates-rollback ((error smb-password-error))
  nil)

(define-persistent-class user ()
  ((login :update
	  :documentation "Unique login name"
	  :index-type string-unique-index
	  :index-reader find-user
	  :index-values all-users)
   (flags :update :initform nil
	  :index-type hash-list-index
	  :index-reader get-flag-users)
   
   (email       :update :initform ""
		:documentation "Email Address, must be unique")
   (full-name   :update :initform "")
   (last-login  :update :initform 0)
   (password    :update :initform "")
   (preferences :read
		:documentation "hash table with user preferences, indexed by keyword"
		:initform (make-hash-table :test #'eq))
   (subscriptions :update :initform nil)
   (mail-error :update
	       :documentation "mail problem the user's mail address - presence indicates that the user is unreachable by mail")))

(defconstant +salt-length+ 8)

(defgeneric user-editable-p (user)
  (:documentation "Return non-nil if the given user can be edited through the administration interface.  The USER class
is frequently subclassed to implement special user accounts that are self-registered and that cannot be edited through
the standard user administration interface.  It would be better if the ``real'' system users would live in a seperate base
class that would be editable and have the USER class be non-editable."))

(defmethod user-editable-p ((user user))
  t)

(defun make-salt ()
  (coerce (loop
	   for i from 1 upto +salt-length+
	   collect (code-char (+ 65 (random 26))))
	  'string))

;;; old crypt function, deprecated
(defun crypt (password &optional (salt (make-salt)))
  "Return string containing md5 encrypted password prepended by the salt.  The salt string
may be longer, so an encrytped password string can be used as salt to verify a given cleartext
password against an encrypted one."
  (setf salt (subseq salt 0 +salt-length+))
  (concatenate 'string salt (md5-string (format nil "~a~a" salt password))))

(defmethod print-object ((object user) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "ID: ~A LOGIN: ~A"
	    (store-object-id object)
	    (if (slot-boundp object 'login)
		(user-login object)
		"unbound"))))

(defmethod initialize-persistent-instance ((user user) &key)
  (let* ((plaintext-password (slot-value user 'password))
	 (password (when plaintext-password (crypt-md5 plaintext-password (make-salt)))))
    (setf (slot-value user 'password) password)))

(defmethod destroy-object :before ((user user))
  (dolist (subscription (user-subscriptions user))
    (delete-object subscription)))

(define-persistent-class smb-user (user)
  ())

(defmethod initialize-persistent-instance ((user smb-user) &key)
  (let* ((plaintext-password (slot-value user 'password)))
    (when plaintext-password
      (set-smb-password (user-login user) plaintext-password))
    (call-next-method)))

(defmethod (setf user-password) :after (plaintext-password (user smb-user))
  (when plaintext-password
    (set-smb-password (user-login user) plaintext-password)))

(defmethod admin-p ((user user))
  (when (user-has-flag user :admin)
    t))

(defmethod user-mail-error-p ((user user))
  (and (slot-boundp user 'mail-error)
       (slot-value user 'mail-error)))

(defmethod user-reachable-by-mail-p ((user user))
  (and (user-email user)
       (not (user-mail-error-p user))))

(defmethod anonymous-p ((user user))
  (string-equal (user-login user) "anonymous"))

(defmethod user-has-flag ((user user) flag)
  (find flag (user-flags user)))

(defvar *user-flags* '(:admin))

(defun define-user-flag (keyword)
  (pushnew keyword *user-flags*))

(defun all-user-flags ()
  (copy-list *user-flags*))

(defmethod verify-password ((user user) password)
  (when password
    (let ((upw (user-password user)))
      (if (equal "$1$" (and (> (length upw) 3) (subseq upw 0 3)))
	  (verify-md5-password password (user-password user))
          (when (> (length upw) +salt-length+)
            (equal upw
                   (crypt password (subseq upw 0 +salt-length+))))))))

(defmethod user-disabled ((user user))
  (user-has-flag user :disabled))

(defmethod user-preferences ((user user))
  "Transitional method to initialize uninitialized slots (XXX should be automatic)"
  (unless (slot-value user 'preferences)
    (setf (slot-value user 'preferences) (make-hash-table :test #'eq)))
  (slot-value user 'preferences))

(defmethod user-preference ((user user) key &optional default)
  (or (gethash key (user-preferences user))
      default))

(deftransaction set-user-last-login (user time)
  (setf (slot-value user 'last-login) time))

(deftransaction set-user-preference (user key value)
  (setf (gethash key (user-preferences user)) value))

(deftransaction user-add-flags (user flags)
  (setf (user-flags user) (union flags (user-flags user))))
					     
(deftransaction user-remove-flags (user flags)
  (setf (user-flags user) (set-difference (user-flags user) flags)))

(defun print-user-passwd-entry (user &key (uid 1) (gid 1) (shell "/bin/false")
				home (base "/home/"))
  (format t "~a:~a:~a:~a::~a:~a~%"
	  (user-login user)
	  (user-password user)
	  uid gid (if home home (concatenate 'string base (user-login user))) shell))

(defun write-passwd-file (pathname &key (base "/home/") (uid 1) (gid 1))
  (with-open-file (s pathname :direction :output :if-exists :supersede
		     :if-does-not-exist :create)
    (let ((*standard-output* s))
      (dolist (user (all-users))
	(print-user-passwd-entry user :uid uid :gid gid :base base))
      (terpri s))))

(defun make-user (login &key password full-name email flags (class 'user))
  (let ((login (string-downcase (string-trim '(#\space) login))))
    (let ((user (make-object class
                             :login login
                             :full-name full-name
                             :flags flags
                             :email (and email (string-downcase email)))))
      (when password
        (set-user-password user password))
      user)))

(defmethod cascade-delete-p ((user user) (event event))
  t)

(defmethod delete-user ((user user))
  (when (eq user (find-user "anonymous"))
    (error "Can't delete system user ``anonymous''"))
  (cascading-delete-object user))

(deftransaction set-user-full-name (user full-name)
  (setf (user-full-name user) full-name))

(deftransaction set-user-crypted-password (user crypted-password)
  (setf (user-password user) crypted-password))

(defgeneric set-user-password (user password)
  (:method ((user user) password)
    (set-user-crypted-password user (crypt-md5 password (make-salt))))
  (:method ((username string) password)
    (set-user-password (find-user username) password)))

;;; owned objects

(define-persistent-class owned-object (store-object)
  ((owner :update :initform nil
          :index-type hash-index
          :index-reader store-object-owner)))

(defmethod convert-slot-value-while-restoring ((object owned-object) (slot-name (eql 'owners)) owners)
  (when owners
    (unless (= 1 (length owners))
      (warn "object ~A has more than one owner ~S, using first" object owners))
    (setf (slot-value object 'owner) (car owners))))

(defgeneric user-owns-object-p (user object))

(defmethod user-owns-object-p ((user user) (object t))
  nil)

(defmethod user-owns-object-p ((user user) (object owned-object))
  (eq user (owned-object-owner object)))

(define-persistent-class message-event (event)
  ((from :read :initform nil)
   (text :read :initform "<empty>"))
  (:documentation "informational message entered by the system or an operator"))

;; message-events

(defmethod message-event-from-name (event)
  (if (message-event-from event)
      (user-login (message-event-from event))
    "<system>"))

(defmethod message-event-from-html-link (event)
  (if (message-event-from event)
      (bknr.web::html-link (message-event-from event))
    (html "&lt;system&gt;")))

(defmethod event-argument ((event message-event))
  (format nil "<~a> ~a"
	  (message-event-from-name event)
	  (message-event-text event)))

(defmethod print-as-html ((event message-event) stream)
  (html-stream stream
	       (:princ-safe (format-date-time (event-time event) :show-year nil))
	       "&nbsp;[message]&nbsp;" (message-event-from-html-link event)
	       "&nbsp;" (:princ-safe (message-event-text event))))

(defmethod as-xml ((event message-event))
  (generate-event-xml event
		      :from (message-event-from-name event)
		      :text (message-event-text event)))
