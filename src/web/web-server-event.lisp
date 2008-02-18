(in-package :bknr.web)

(define-persistent-class web-server-log-event (event)
  ((host :read :relaxed-object-reference t)
   (user :read :relaxed-object-reference t)
   (session-id :read)
   (url :read)
   (referer :read :initform nil)
   (user-agent :read :initform nil))
  (:documentation "Web page has been served"))

(defmethod print-object ((event web-server-log-event) stream)
  (with-slots (session-id host user url) event
    (format stream "#<~a at ~a session-id ~a host ~a user ~a url ~S>"
	    (class-name (class-of event)) (format-date-time (event-time event))
	    session-id host user url))
  event)

(defun all-web-server-log-events ()
  (store-objects-with-class 'web-server-log-event))

(defun web-server-log-event-p (event)
  (typep event 'web-server-log-event))

(define-persistent-class web-server-error-event (web-server-log-event)
  ((error :read)
   (backtrace :read))
  (:documentation "Backtrace when an error happens inside a web page"))

(defmethod print-object ((event web-server-error-event) stream)
  (format stream "#<~a at ~a error ~a>"
	  (class-name (class-of event))
	  (format-date-time (event-time event))
	  (web-server-error-event-error event))
  event)

(defun all-web-server-error-events ()
  (store-objects-with-class 'web-server-error-event))

