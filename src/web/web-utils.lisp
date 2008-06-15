(in-package :bknr.web)

(enable-interpol-syntax)

(defstruct upload name pathname original-filename content-type)

(defgeneric object-url (obj))
(defgeneric edit-object-url (obj))
(defgeneric html-link (obj))
(defgeneric html-edit-link (obj))

(defparameter *upload-file-size-limit* 5000000)

(defun error-404 ()
  (with-http-response (:response +http-not-found+)
    (with-http-body ()
      (html "The page you requested could not be found."))))

(defun redirect-uri (uri)
  (make-instance 'uri :path (uri-path uri)
		 :query (uri-query uri)))

(defun request-uploaded-files ()
  "Return a list of UPLOAD structures describing the file uploads in the request."
  (unless (aux-request-value 'uploaded-files)
    (setf (aux-request-value 'uploaded-files)
          (let ((uploads (remove-if-not #'listp (post-parameters) :key #'cdr)) retval)
            (dolist (upload-info uploads)
              (destructuring-bind (name pathname original-filename content-type) upload-info
                (push (make-upload :name name :pathname pathname :original-filename original-filename :content-type content-type) retval)))
            (nreverse retval))))
  (aux-request-value 'uploaded-files))

(defun request-uploaded-file (parameter-name)
  (find parameter-name (request-uploaded-files) :test #'equal :key #'upload-name))

(defmacro with-image-from-upload ((image upload &rest args) &body body)
  `(with-image-from-file (,image (upload-pathname ,upload)
                          (make-keyword-from-string (pathname-type (upload-original-filename ,upload))) ,@args)
    ,@body))

(defmacro with-image-from-upload* ((upload &rest args) &body body)
  `(with-image-from-upload (cl-gd:*default-image* ,upload ,@args)
    ,@body))

(defmethod bknr.images:import-image ((upload upload) &rest args &key &allow-other-keys)
  (apply #'bknr.images:import-image (upload-pathname upload)
         :name (pathname-name (upload-original-filename upload))
         :type (make-keyword-from-string (pathname-type (upload-original-filename upload))) args))

(defun all-request-params ()
  "Return all non-empty request parameters - This includes all parameters encoded in the URL as
well as those in the request body, either as urlencoded strings or as multipart body.  If a multipart
body is present in the request, any uploaded files are saved in a temporary file and noted in the
request's plist.  Uploaded files will be automatically deleted by the with-http-response
macro after the request body has been executed."
  (unless (aux-request-value 'bknr-parsed-parameters)
    (setf (aux-request-value 'bknr-parsed-parameters)
	    (remove-if (lambda (value)
                         "Remove empty strings (reported as NIL) and uploaded files"
                         (or (equal value "")
                             (listp value)))
                       (query-params)
                       :key #'cdr)))
  (aux-request-value 'bknr-parsed-parameters))

(defun query-params (&key (get t) (post t))
  (append (when get (get-parameters))
          (when post (post-parameters))))

(defun query-param (param-name &key (get t) (post t))
  (let ((value (cdr (assoc param-name (query-params :get get :post post) :test #'equal))))
    (unless (equal value "")
      value)))

(defun query-param-list (param-name &key (get t) (post t))
  (assoc-values param-name (query-params :get get :post post)
                :test #'string-equal))

(defun request-variable (var)
  (gethash var *req-var-hash*))

(defun (setf request-variable) (new-value var)
  (setf (gethash var *req-var-hash*) new-value))

(defun request-variables ()
  (loop for key being the hash-keys of *req-var-hash*
	collect key
	collect (request-variable key)))

(defun http-error (response message)
  (with-bknr-page (:title #?"error: $(message)" :response response)
    (:princ-safe message))
  (error message))

(defun keywords-from-query-param-list (param &key (remove-empty t))
  (let ((keywords (mapcar #'(lambda (s)
			      (make-keyword-from-string (string-trim '(#\Space #\Tab #\Newline) s)))
			  param)))
    (if remove-empty
	(remove-if #'(lambda (x) (eq x :||)) keywords)
	keywords)))

(defun html-quote (string)
  (regex-replace-all "([&<>])" string #'(lambda (target-string start end match-start &rest args)
					  (declare (ignore start end args))
					  (ecase (elt target-string match-start)
					    (#\& "&amp;")
					    (#\< "&lt;")
					    (#\> "&gt;")))))

(defun parse-url ()
  (values-list (cddr (mapcar #'url-decode (split "/" (script-name*))))))

(defun last-url-component ()
  (register-groups-bind (last)
      ("/([^\\/]+)$" (script-name*))
    last))

(defun parse-date-field (name)
  (let ((timespec (mapcar #'(lambda (var) (parse-integer
					   (query-param (concatenate 'string name "-" var))
					   :junk-allowed t))
			  '("minute" "hour" "day" "month" "year"))))
    (unless (car timespec)
      (rplaca timespec 0))
    (unless (cadr timespec)
      (rplaca (cdr timespec) 0))
    (if (every #'identity timespec)
	(apply #'encode-universal-time 0 timespec)
	nil)))

(defun bknr-url-path (handler)
  "Returns the Path of the request under the handler prefix"
  (let ((len (length (page-handler-prefix handler))))
    (subseq (script-name*) len)))

(defun self-url (&key command prefix)
  (destructuring-bind
	(empty old-prefix object-id &rest old-command)
      (split "/" (script-name*))
    (declare (ignore empty))
    #?"/$((or prefix old-prefix))/$(object-id)/$((or command old-command))"))

(defmethod html-link ((object store-object))
  (html (:princ (format nil "[persistent object with id #~a]" (store-object-id object)))))

(defun text-to-html (string)
  "Perform simple text to HTML conversion.  http urls are replaced by links, internal links to
images become image tags."
  (setf string (regex-replace-all
		#?r"bknr:([0-9A-Za-z$-_.+!*'()]+)" string
		#'(lambda (target-string start end match-start match-end reg-starts reg-ends)
		    (declare (ignore start end match-start match-end))
		    (let ((url (subseq target-string (aref reg-starts 0) (aref reg-ends 0))))
		      (regex-replace-all "URL" (if (all-matches "^/image" url)
						   "<img src=\"URL\" />"
						   "<a href=\"URL\">URL</a>")
					 url)))))
  (setf string (regex-replace-all
		#?r"(http://[0-9A-Za-z$-_.+!*'()]+)" string
		#'(lambda (target-string start end match-start match-end &rest args)
		    (declare (ignore start end args))
		    (let ((url (subseq target-string match-start match-end)))
		      (regex-replace-all "URL" (if (all-matches "(?i)\\.(gif|jpe?g|png)$" url)
						   "<img src=\"URL\" />"
						   "<a href=\"URL\" target=\"_blank\">URL</a>")
					 url)))))
  (setf string (regex-replace-all "[\\r\\n]" string "<br>"))
  string)

(defun make-wiki-hrefs (string)
  (regex-replace-all #?r"\[(.+?)\]" string
		     #'(lambda (target-string start end match-start match-end
				reg-starts reg-ends)
			 (declare (ignore start end match-start match-end))
			 (let ((keyword (subseq target-string
						(svref reg-starts 0)
						(svref reg-ends 0))))
			   (format nil "<a class=\"wikilink\" href=\"/wiki/~a\">~a</a>"
				   keyword
				   keyword)))))

(defmacro bknr-handler-case (body &rest handler-forms)
  `(if *bknr-debug*
    ,body
    (handler-case
	,body
      ,@handler-forms)))

(defun emit-element-attributes (attributes)
  (loop for (key value) on attributes by #'cddr
        do (progn
             (princ " ")
             (princ (string-downcase (symbol-name key)))
             (princ "=\"")
             (princ value)
             (princ "\""))))

(defun emit-html (&rest forms)
  (let ((element (car forms)))
    (etypecase element
      ;; :foo
      (keyword (handle-tag element nil nil))
      ;; (:foo ...) or ((:foo ...) ...)
      (cons (if (consp (car element))
                (handle-tag (caar element) (cdar element) (cdr element)) ; ((:foo ...) ...)
                (handle-tag (car element) nil (cdr element))))           ; (:foo ...)
      ;; "foo"
      (string (princ element))))
  (when (cdr forms)
    (apply #'emit-html (cdr forms))))

(defun handle-tag (tag-symbol attributes body)
  ;; emit xhtml
  (let ((tag-name (string-downcase (symbol-name tag-symbol))))
    ;; emit < and tag name
    (princ "<")
    (princ tag-name)
    ;; emit possible attributes
    (when attributes
      (emit-element-attributes attributes))
    (if body
	;; emit tag body
	(progn
	  (princ ">")
	  (apply #'emit-html body)
	  (princ "</")
	  (princ tag-name)
	  (princ ">"))
	;; empty body, close tag immediately
	(princ " />"))))

(defun encode-urlencoded (string)
(regex-replace-all #?r"\+" (url-encode string) "%20"))
