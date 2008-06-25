(in-package :bknr.web.frontend)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun read-template (path)
    (cl-interpol:enable-interpol-syntax)
    (unwind-protect
	 (with-open-file (in path)
	   (read in))   
      (cl-interpol:disable-interpol-syntax))))

(defun cachable-prefixes-regex ()
  (format nil "^(~{~A~^|~})" (mapcar #'page-handler-prefix (website-cachable-handlers *website*))))

(defun generate-frontend-config (stream &key
				 backend-port)  
  (check-type backend-port (integer 0))
  (let ((cachable-prefixes-regex (cachable-prefixes-regex)))
    (write-sequence #.(read-template (merge-pathnames "config-template.lisp-expr"
						      *compile-file-pathname*))
		    stream))
  (values))

;;; quick hack
;;; as this file depends on config-template.lisp-expr
;;; we dont want to produce a stale fasl
;;; better to use ASDF
(eval-when (:load-toplevel)
  (let ((fasl-path (compile-file-pathname *load-pathname*)))
    (when (probe-file fasl-path)
      (delete-file fasl-path))))

