(in-package :bknr.web.frontend)

(define-condition frontend-already-running (simple-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
	     (format stream "BKNR web frontend is already running."))))

(defvar *frontend-pid-file* #p "/tmp/bknr-web-frontend.pid")

(defun read-pid-file ()
  (with-open-file (in *frontend-pid-file*)
    (read in)))

(defun front-end-running-p ()
  (and (probe-file *frontend-pid-file*)
       (zerop (asdf:run-shell-command "sudo kill -0 ~D" (read-pid-file)))))

(defun start-frontend (&key host backend-port (port 80))
  (when (front-end-running-p)
    (cerror "Tear it down!" 'frontend-already-running)
    (stop-frontend :verbose t)
    (assert (not (front-end-running-p)) nil
	    "Failed to stop frontend. This is a bug."))
  (let* ((cmd (format nil "sudo varnishd -a ~A:~D ~
                                   -b localhost:~D ~
                                   -P '~A'"
		      (or host "") port
		      backend-port
		      *frontend-pid-file*))
	 (exit-code (progn (format t "; $ ~A" cmd)
			   (asdf:run-shell-command cmd))))
    (unless (zerop exit-code)
      (error "Attempt to launch varnishd returned error ~D." exit-code)))
  (values))

(defun stop-frontend (&key verbose)
  (if (front-end-running-p)
      (progn
	(when verbose (format t "~&; Stopping frontend with PID ~D..." (read-pid-file)))
	(asdf:run-shell-command "sudo kill -9 ~D" (read-pid-file))
	(when verbose (format t "done.~%")))
      (warn "Frontend is not running."))
  (values))

