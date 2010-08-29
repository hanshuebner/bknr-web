(in-package :bknr-user)

(cl-interpol:enable-interpol-syntax)

(defun obex-push-stream (btaddr in-stream &optional filename)
  (let ((temp-file (make-temporary-pathname :defaults "/tmp/")))
    (unwind-protect
	 (progn
	   (with-open-file (s temp-file :direction :output
			      :element-type '(unsigned-byte 8)
			      :if-exists :error :if-does-not-exist :create)
	     (copy-stream in-stream s))
	   (let ((output
		  (with-output-to-string (stream)
		    (let ((proc (run-program #+nil"/home/manuel/foobar.sh"
					     "/home/manuel/sense/sense"
					     `("-n" "60"
					       "/usr/local/bin/obexapp"
					       "-n" "-a" ,btaddr "-C" "OPUSH"
					       "put" ,(namestring temp-file)
					       ,@(when filename (list filename)))
					     :wait t
					     :output stream
					     :error :output)))
		      (when (eql (process-status proc) :signaled)
			(return-from obex-push-stream nil))
		      (unless (eql (process-status proc) :exited)
			(error "Could not start obexapp, status: ~A" (process-status proc)))))))
	     (format t "output of obexapp: ~A~%" output)
	     ;;; IEEEEHHH, der setzt den return code nicht
	     (unless (or (scan #?r"Response" output)
			 (scan #?r"failed" output))
	       t)))
      (delete-file temp-file))))

(defun obex-push-file (btaddr in-file &optional filename)
  (unless filename
    (setf filename (namestring (make-pathname :name (pathname-name in-file)
					      :type (pathname-type in-file)))))
  (with-open-file (s in-file :direction :input
		     :element-type '(unsigned-byte 8))
    (obex-push-stream btaddr s filename)))

(defun bluetooth-inquiry (&optional (dev "ubt0hci"))
  (let ((str (with-output-to-string (stream)
	       (let ((proc (run-program "hccontrol" `("-n" ,dev "inquiry")
					:wait t
					:output stream
					:error :output)))
		 (unless (eql (process-status proc) :exited)
		   (error "Could not start hccontrol")))))
	(addresses))
    (format t "inquiry output: ~A~%" str)
    (do-scans (match-start match-end regs-start regs-end
			   #?r"BD_ADDR:\s*([^\s]+)" str)
      (let ((addr-start (aref regs-start 0))
	    (addr-end (aref regs-end 0)))
	(pushnew (subseq str addr-start addr-end) addresses)))
    addresses))

(defun bluetooth-has-opush (addr)
  (let ((str (with-output-to-string (stream)
	       (let ((proc (run-program "sdptool" `("search"
						    "--bdaddr" ,addr
						    "OPUSH")
					:wait t
					:output stream)))
		 (unless (eql (process-status proc) :exited)
		   (error "COuld not start sdptool"))))))
    (when (scan #?r"Channel:\s*[0-9]+" str)
      t)))

(defun spam-server (filename)
  (let ((seen-devices (make-hash-table :test #'equal))
	(sent 0))
    (loop
     (format t "Starting inquiry...~%")
     (force-output)

     (sleep 1)
     (let ((devs (bluetooth-inquiry)))
       (format t "found devs: ~A~%" devs)
       (dolist (dev #+nil(list *dev*)
		    devs)
	 (format t "Examining ~A~%" dev)
	 (force-output)
	 (if (gethash dev seen-devices)
	     (progn
	       (format t "Ignoring already seen device ~A~%" dev)
	       (force-output))
	     (if (bluetooth-has-opush dev)
		 (progn
		   (format t "~A has OPUSH, sending image ~A~%" dev filename)
		   (force-output)
		   (ignore-errors (obex-push-file dev filename))
		   (setf (gethash dev seen-devices) t)
		   (incf sent)
		   (format t "Sent ~A to dev ~A~%" filename dev)
		   (format t "Sent ~a images in total.~%" sent)
		   (force-output))
		 (progn
		   (format t "Ignoring ~A without OPUSH~%" Dev)
		   (force-output))
		 )))))))
