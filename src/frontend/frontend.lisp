(in-package :bknr.web.frontend)

(define-condition frontend-already-running (simple-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
	     (format stream "BKNR web frontend is already running."))))

(defvar *varnish-directory*)

(defparameter *frontend-verbose* nil)
(defparameter *frontend-debug* nil)

(defun frontend-pid-file ()
  (merge-pathnames #P"bknr-web-frontend.pid" *varnish-directory*))

(defun frontend-config-file ()
  (merge-pathnames #P"config.vcl" *varnish-directory*))

(defun read-pid-file (&key (errorp t))
  (uiop:run-program `("sudo" "chmod" "644" ,(uiop:native-namestring (frontend-pid-file))))
  (let ((pid (with-open-file (in (frontend-pid-file))
	       (read in nil))))
    (when (and (null pid) errorp)
      (error "pid-file is empty"))
    pid))

(defun frontend-version ()
  (handler-case
      (let ((varnishd-v (uiop:run-program '("varnishd" "-V") :output :string)))
	(or (when (ppcre:scan "varnish-trunk" varnishd-v) 'trunk)
	    (ppcre:register-groups-bind (version-string)
		("varnish-(\\d+(?:\\.\\d+)+)" varnishd-v)
	      (mapcar #'parse-integer  (ppcre:split "\\." version-string)))
	    (error 'error)))
    (error () (error "Cannot determine frontend version."))))

(defun frontend-running-p ()
  (and (probe-file (frontend-pid-file))
       (read-pid-file :errorp nil)	; pid file might be empty
       (zerop
        (nth-value 2 (uiop:run-program `("sudo" "kill" "-0" ,(princ-to-string (read-pid-file)))
                                       :ignore-error-status t)))))

(defun stop-frontend (&key verbose)
  (cond
    ((frontend-running-p)
     (when verbose (format t "~&; Stopping frontend with PID ~D..." (read-pid-file)))
     (uiop:run-program `("sudo" "kill" ,(princ-to-string (read-pid-file))))
     (dotimes (i 10
               (error "Frontend could not be stopped"))
       (unless (frontend-running-p)
         (return)))
     (when verbose (format t "done.~%")))
    (t
     (warn "Frontend is not running.")))
  (values))

(defun start-frontend (&key
                       host
                       backend-port
                       (verbose t)
                       (varnish-directory (pathname (format nil "/tmp/varnish-~A/" backend-port)))
                       (port 80))
  (let ((*varnish-directory* (ensure-directories-exist varnish-directory))
        (*frontend-verbose* verbose))
    ;; check frontend version
    (assert (equal 'trunk (frontend-version)))
    ;; generate config file
    (with-open-file (out (frontend-config-file) :direction :output :if-exists :supersede)
      (generate-frontend-config out :backend-port backend-port))
    ;; stop running frontend if needed
    (when (frontend-running-p)
      #+(or) (cerror "Stop the running frontend process" 'frontend-already-running)
      (stop-frontend :verbose verbose)
      (assert (not (frontend-running-p)) nil
              "Failed to stop frontend. This is a bug."))
    ;; start frontend
    (when verbose
      (format t "; Starting varnishd frontend process on ~@[~A~]:~A~%" host port))
    (let ((exit-code (nth-value 2 (uiop:run-program
                                   `("sudo" "varnishd" "-a"
                                     ,(format nil "~@[~A~]:~D" host port)
                                     "-f" ,(uiop:native-namestring (frontend-config-file))
                                     "-n" ,(uiop:native-namestring varnish-directory)
                                     "-P" ,(uiop:native-namestring (frontend-pid-file)))))))
      (unless (zerop exit-code)
        (error "Attempt to launch varnishd exit code ~D.~
              ~%Among other possible reasons, it might be that:~
              ~%  - you are not allowed to run sudo~
              ~%  - varnishd could not successfully compile the VCL config~%"
	       exit-code))
      (when verbose
	(format t "; varnishd has PID ~D" (read-pid-file))))
    (values)))

