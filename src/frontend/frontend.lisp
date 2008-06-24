(in-package :bknr.web.frontend)

(define-condition frontend-already-running (simple-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
	     (format stream "BKNR web frontend is already running."))))

(defvar *frontend-pid-file* #P"bknr-web-frontend.pid")

(defun read-pid-file ()
  (with-open-file (in *frontend-pid-file*)
    (read in)))

(defun frontend-running-p ()
  (and (probe-file *frontend-pid-file*)
       (zerop (asdf:run-shell-command "sudo kill -0 ~D" (read-pid-file)))))

(defun start-frontend (&key
                       host
                       backend-port
                       (varnish-directory (namestring (format nil "/tmp/varnish-~A/" backend-port)))
                       (port 80))
  (when (frontend-running-p)
    (cerror "Tear it down!" 'frontend-already-running)
    (stop-frontend :verbose t)
    (assert (not (frontend-running-p)) nil
	    "Failed to stop frontend. This is a bug."))
  (let* ((cmd (format nil "sudo varnishd -a ~@[~A~]:~D ~
                                   -b localhost:~D ~
                                   -n '~A' ~
                                   -P '~A'"
		      host port
		      backend-port
                      varnish-directory
		      *frontend-pid-file*))
	 (exit-code (progn (format t "; $ ~A" cmd)
			   (asdf:run-shell-command cmd))))
    (unless (zerop exit-code)
      (error "Attempt to launch varnishd returned error ~D." exit-code)))
  (values))

(defun stop-frontend (&key verbose)
  (cond
    ((frontend-running-p)
     (when verbose (format t "~&; Stopping frontend with PID ~D..." (read-pid-file)))
     (asdf:run-shell-command "sudo kill ~D" (read-pid-file))
     (dotimes (i 10
               (error "Frontend could not be stopped"))
       (unless (frontend-running-p)
         (return)))
     (when verbose (format t "done.~%")))
    (t
     (warn "Frontend is not running.")))
  (values))

