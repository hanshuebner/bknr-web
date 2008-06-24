(in-package :bknr.web.frontend)

(define-condition frontend-already-running (simple-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
	     (format stream "BKNR web frontend is already running."))))

(defvar *varnish-directory*)

(defparameter *frontend-verbose* nil)
(defparameter *frontend-debug* nil)

(defun execute-shell (fmt &rest args)
  (let ((command (apply #'format nil fmt args)))
    (when *frontend-debug*
      (format t "; $ ~A~%" command))
    (asdf:run-shell-command command)))

(defun frontend-pid-file ()
  (merge-pathnames #P"bknr-web-frontend.pid" *varnish-directory*))

(defun read-pid-file ()
  (asdf:run-shell-command (format nil "sudo chmod 644 ~A" (namestring (frontend-pid-file))))
  (with-open-file (in (frontend-pid-file))
    (read in)))

(defun frontend-running-p ()
  (and (probe-file (frontend-pid-file))
       (zerop (execute-shell "sudo kill -0 ~D" (read-pid-file)))))

(defun stop-frontend (&key verbose)
  (cond
    ((frontend-running-p)
     (when verbose (format t "~&; Stopping frontend with PID ~D..." (read-pid-file)))
     (execute-shell "sudo kill ~D" (read-pid-file))
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
  (let ((*varnish-directory* varnish-directory)
        (*frontend-verbose* verbose))
    (when (frontend-running-p)
      #+(or) (cerror "Stop the running frontend process" 'frontend-already-running)
      (stop-frontend :verbose verbose)
      (assert (not (frontend-running-p)) nil
              "Failed to stop frontend. This is a bug."))
    (when verbose
      (format t "; Starting varnishd frontend process on ~@[~A~]:~A~%" host port))
    (let ((exit-code (execute-shell "sudo varnishd -a ~@[~A~]:~D ~
                                   -b localhost:~D ~
                                   -n '~A' ~
                                   -P '~A'"
                                    host port
                                    backend-port
                                    (namestring varnish-directory)
                                    (namestring (frontend-pid-file)))))
      (unless (zerop exit-code)
        (error "Attempt to launch varnishd exit code ~D." exit-code)))
    (values)))

