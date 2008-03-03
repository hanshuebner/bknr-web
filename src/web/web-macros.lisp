(in-package :bknr.web)

(enable-interpol-syntax)

(defvar *bknr-debug* nil)
(defvar *website* nil)

(defvar *website-modules* (make-hash-table :test #'equal))

(defvar *session* nil "Current session")
(defvar *user* nil "Current user")
(defvar *req-var-hash* nil "Request variables")

(defmacro with-bknr-page ((&rest args) &body body)
  `(show-page-with-error-handlers (lambda () (html ,@body)) ,@args))

(defmacro with-cookies ((&rest names) &rest body)
  `(let ,(mapcar #'(lambda (name)
		     `(,name (cookie-in ,(symbol-name name))))
		 names)
    ,@body))

(defmacro with-query-params ((&rest params) &rest body)
  (let ((vars (loop for param in params
		    when (and (symbolp param)
			      (not (null param)))
		    collect (list param `(query-param ,(string-downcase (symbol-name param))))
		    when (consp param)
		    collect (list (car param)
				  `(or (parameter ,(string-downcase (symbol-name (car param))))
				    ,(second param))))))
    (if vars
	`(let ,vars
	  ,@body)
	(first body))))

(defmacro form-case (&rest cases)
  `(cond
    ,@(mapcar #'(lambda (c)
		  (if (eql (car c) t)
		      `(t ,@(cdr c))
		      `((parameter ,(symbol-name (car c)))
			(with-query-params (,@(cadr c))
			  ,@(cddr c)))))
	      cases)))

(defmacro with-http-response ((&key (content-type "text/html") (response +http-ok+)) &rest body)
  `(progn
    (setf (content-type) ,content-type)
    (setf (return-code) ,response)
    ,@body))

(defmacro with-http-body ((&key external-format) &body body)
  `(with-output-to-string (stream)
    (with-xhtml (stream)
      ,@body)))

(defmacro with-image-from-uri ((image-variable prefix) &rest body)
  `(multiple-value-bind
    (match strings)
    (scan-to-strings (format nil "/~a/([0-9]+)(|/.*)$" ,prefix) (script-name))
    (unless match
      (http-error +http-bad-request+ "bad request - missing image path or loid"))
    (let ((,image-variable (store-object-with-id (parse-integer (elt strings 0)))))
      (unless ,image-variable
	(http-error +http-not-found+ "image not found"))
      ,@body)))

(defmacro define-bknr-tag (name (&rest args) &rest body)
  `(prog1
    (defun ,name (,@args)
      ,@body)
    (register-tag-function ,(package-name *package*) ,(symbol-name name) (fdefinition ',name))))

(defmacro html-text-input (variable size &optional maxsize)
  `((:input :type "text" 
	    :size ,(format nil "~a" size)
	    :maxsize ,(format nil "~a" (or maxsize size))
	    :name ,(symbol-name variable)
	    :value ,(or variable ""))))

(defmacro html-warn (&rest warning)
  "Generate a warning on the console and write the warning into the
currently generated XHTML output as a comment."
  `(progn
    (html (:princ-safe (format nil "<!-- ~a -->~%" (format nil ,@warning))))
    (warn ,@warning)))

(defmacro cmslink (url &body body)
  `(html ((:a :class "cmslink" :href (website-make-path *website* ,url))
	  ,@body)))
