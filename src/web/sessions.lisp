(in-package :bknr.web)

(defclass bknr-session ()
  ((id :initarg :id :reader bknr-session-id :initform (get-universal-time))
   (user :initarg :user)
   (host :initarg :host :reader bknr-session-host :initform nil)))

(defmethod print-object ((session bknr-session) stream)
  (print-unreadable-object (session stream :type t :identity t)
    (with-slots (user host) session
      (format stream "user ~A host ~A" user host))
    session))

(defun bknr-session ()
  (session-value 'bknr-session))

(defun bknr-session-user ()
  (slot-value (bknr-session) 'user))

(defun do-log-request ()
  (format *debug-io* "Log: ~A~%" (script-name*))
  (return-from do-log-request)
  #+(or)
  (let* ((session (bknr-session))
	 (user (bknr-session-user session))
	 (host (bknr-session-host session))
	 (url (script-name*))
	 (referer (header-in :referer))
	 (user-agent (header-in :user-agent))
	 (time (get-universal-time)))
    (prog1
	(make-event 'web-server-log-event
		    :time time
		    :host host
		    :user user
		    :referer referer
		    :user-agent user-agent
		    :session-id (bknr-session-id session)
		    :url url)
      (format t "; ~A ~A ~A ~A~%" (format-date-time time)
	      (if user (user-login user) "anonymous") (host-name host) url))))

(defun do-error-log-request (error)
  (format *debug-io* "Error: ~A~%" error)
  #+(or)
  (let* ((session (bknr-session))
	 (user (bknr-session-user session))
	 (host (bknr-session-host session))
	 (url (script-name*))
	 (referer (header-in :referer))
	 (time (get-universal-time)))
    (make-event 'web-server-error-event
		:time time
		:host host
		:user user
		:referer referer
		:session-id (bknr-session-id session)
		:url url
		:error (format nil "~A" error)
		:backtrace
		#+allegro
		""
		#+cmu
		(with-output-to-string (s)
		  (debug:backtrace 30 s))
		#+sbcl
		(with-output-to-string (s)
		  (sb-debug:backtrace 30 s)))))
