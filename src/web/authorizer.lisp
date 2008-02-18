(in-package :bknr.web)

(defclass bknr-authorizer ()
  ())

(defmethod http-request-remote-host ()
  (format *debug-io* "can't determin originating host yet~%")
  #+(or)
  (let ((remote-host (socket:remote-host (request-socket)))
	(forwarded-for (regex-replace
			"^.*?([0-9]+\.[0-9]+\.[0-9]+\.[0-9]+).*$"
			(header-slot-value :x-forwarded-for)
			"\\1")))
    (when (and forwarded-for
	       (equal "127.0.0.1" (socket:ipaddr-to-dotted remote-host)))
      ;; request is via proxy, use client's ip address
      (setf remote-host (socket:dotted-to-ipaddr forwarded-for)))
    (find-host :create t :ipaddr remote-host)))

(defun session-from-request ()
  "check whether the request has a valid session id in either the bknr-sessionid cookie or query parameter"
  (session-value 'bknr-session))

(define-condition login-failure (serious-condition)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Login failed"))))

(defun find-user-from-request-parameters ()
  (with-query-params (__username __password)
    (unless (and __username __password
                 (not (equal __username ""))
                 (not (equal __password "")))
      (return-from find-user-from-request-parameters nil))
    (let ((user (find-user __username)))
	(when (and user
                   (not (user-disabled user))
		   (verify-password user __password))
          (return-from find-user-from-request-parameters user)))
    (error 'login-failure)))

(defmethod authorize ((authorizer bknr-authorizer))
  ;; Catch any errors that occur during request body processing
  (handler-case
      (when (session-value 'bknr-session)
	(return-from authorize t))
    (error (e)
      (format t "; Caught error ~A during request processing~%" e)
      (setf (return-code) +http-bad-request+)
      (princ-to-string e)))

  ;; unauthorized, come up with 401 response to the web browser
  (redirect "/login")
  :deny)
