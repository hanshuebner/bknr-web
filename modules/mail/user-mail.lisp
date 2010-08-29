(in-package :bknr.mail)

(enable-interpol-syntax)

(define-condition user-mail-condition (error)
  ((user :initarg :user :reader user-mail-condition-user)))

(define-condition user-mail-unreachable-condition (user-mail-condition)
  ())

(defmethod print-object ((condition user-mail-unreachable-condition) stream)
  (let ((user (user-mail-condition-user condition)))
    (format stream "#<~a: user ~a is not ~@[currently ~*~]reachable by email~@[ at ~a~]>"
	    (class-name (class-of condition))
	    (user-login user)
	    (user-mail-error-p user)
	    (user-email user)))
  condition)

(defmethod user-send-mail ((user user) (mail mail) &rest mail-args)
  (unless (user-reachable-by-mail-p user)
    (error (make-condition 'user-mail-unreachable-condition
			   :user user)))
  (apply #'send-mail mail (user-email user) mail-args))

