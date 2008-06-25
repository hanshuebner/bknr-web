(in-package :bknr.web.frontend)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun read-template (path)
    (cl-interpol:enable-interpol-syntax)
    (unwind-protect
	 (with-open-file (in path)
	   (read in))   
      (cl-interpol:disable-interpol-syntax))))

(defun generate-frontend-config (stream &key
				 backend-port)  
  (check-type backend-port (integer 0))
  (write-sequence #.(read-template (merge-pathnames "config-template.lisp-expr"
						    *compile-file-pathname*))
		  stream)
  (values))

