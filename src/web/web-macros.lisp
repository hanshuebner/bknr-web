(in-package :bknr.web)

(enable-interpol-syntax)

(defvar *bknr-debug* nil)
(defvar *website* nil)

(defvar *website-modules* (make-hash-table :test #'equal))

(defvar *session* nil "Current session")
(defvar *req-var-hash* nil "Request variables")

(defmacro with-bknr-page ((&rest args) &body body)
  `(show-page-with-error-handlers (lambda () (html ,@body)) ,@args))

(defmacro with-cookies ((&rest names) &body body)
  `(let ,(mapcar #'(lambda (name)
                     `(,name (cookie-in ,(symbol-name name))))
                 names)
     ,@body))

(defmacro with-query-params ((&rest parameters) &body body)
  "PARAMETERS is a list of parameter-specifiers. A parameter-specifier
has the form (VARIABLE &OPTIONAL DEFAULT-VALUE TYPE) or can be a
single VARIABLE.

If the TYPE is specified, the value is converted like in
HUNCHENTOOT:DEFINE-EASY-HANDLER when PARAMETER-TYPE is given.

With respect to the conversion of an empty string, there is a subtle
difference between the TYPE specified as STRING and the TYPE left
unspecified. In the former case, the converted value will still be an
empty string, while in the latter VARIABLE will be bound to NIL."
  (flet ((parameter-binding (parameter-specifier)
           (destructuring-bind (variable &optional default-value type)
               (ensure-list parameter-specifier)
             (let ((query-param-form (if type
                                         `(query-param ,(string-downcase variable) :type ',type)
                                         `(query-param ,(string-downcase variable)))))
               `(,variable
                 ,(if default-value
                      `(or ,query-param-form ,default-value)
                      query-param-form))))))
    `(let ,(mapcar #'parameter-binding parameters)
       ,@body)))

(defmacro form-case (&rest cases)
  `(cond
     ,@(mapcar #'(lambda (c)
                   (if (eql (car c) t)
                       `(t ,@(cdr c))
                       `((parameter ,(symbol-name (car c)))
                         (with-query-params (,@(cadr c))
                           ,@(cddr c)))))
               cases)))

(defmacro with-http-response ((&key (content-type "text/html") (response +http-ok+)) &body body)
  `(progn
     (setf (content-type*) ,content-type)
     (setf (return-code*) ,response)
     ,@body))

(defmacro with-http-body ((&key external-format) &body body)
  (when external-format
    (warn "EXTERNAL-FORMAT is ignored in WITH-HTTP-BODY"))
  `(with-output-to-string (stream)
     (with-xhtml (stream)
       ,@body)))

(defmacro with-image-from-uri ((image-variable prefix) &body body)
  `(multiple-value-bind
         (match strings)
       (scan-to-strings (format nil "/~a/([0-9]+)(|/.*)$" ,prefix) (script-name*))
     (unless match
       (http-error +http-bad-request+ "bad request - missing image path or loid"))
     (let ((,image-variable (store-object-with-id (parse-integer (elt strings 0)))))
       (unless ,image-variable
         (http-error +http-not-found+ "image not found"))
       ,@body)))

(defmacro define-bknr-tag (name (&rest args) &body body)
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

(defvar *xml-sink*)

(defmacro with-xml-response ((&key (content-type "text/xml; charset=utf-8") root-element xsl-stylesheet-name)
                             &body body)
  `(with-http-response (:content-type ,content-type)
     (with-query-params (download)
       (when download
         (setf (hunchentoot:header-out :content-disposition)
               (format nil "attachment; filename=~A" download))))
     (with-output-to-string (s)
       (let ((*xml-sink* (cxml:make-character-stream-sink s :canonical nil)))
         (cxml:with-xml-output *xml-sink*
           ,(when xsl-stylesheet-name
                  `(sax:processing-instruction *xml-sink* "xml-stylesheet"
                                               ,(format nil "type=\"text/xsl\" href=\"~A\"" xsl-stylesheet-name)))
           ,(if root-element
                `(cxml:with-element ,root-element
                   ,@body)
                `(progn ,@body)))))))