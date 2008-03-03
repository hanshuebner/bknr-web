(in-package :bknr.web)

(enable-interpol-syntax)

(defstruct upload name pathname content-type)

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

#+(or)
(defun get-multipart-form-data ()
  (unless (aux-request-value 'multipart-parsed)
    (let (parameters
	  uploaded-files
	  file-size-limit-reached)
      (loop
       (multiple-value-bind (kind part-name file-name content-type)
	   (parse-multipart-header (get-multipart-header request))
	 (case kind
	   (:eof (return))
	   (:data (push (cons part-name (get-all-multipart-data request)) parameters))
	   (:file (let ((contents (get-all-multipart-data request
							  :type :binary
							  :limit *upload-file-size-limit*))
			(file-basename (regex-replace #?r".*[\\/]" file-name "")))
		    (cond
		      ((eq contents :limit)
		       (setf file-size-limit-reached t))
		      ((equal file-name "")
		       (warn "Can't parse file name from uploaded file named ~a, file ignored" file-name))
		      (t
		       (let ((uploaded-file-name (merge-pathnames file-basename (store-blob-root-tempdir))))
			 (format t "; writing uploaded file ~a to ~a~%" file-name uploaded-file-name)
			 (ensure-directories-exist (store-blob-root-tempdir))
			 (with-open-file (temporary-file uploaded-file-name
							 :direction :output
							 :if-exists :error
							 :element-type '(unsigned-byte 8))
			   (write-sequence contents temporary-file))
			 (push (make-upload :name part-name :pathname uploaded-file-name
					    :content-type content-type) uploaded-files))))))
	   (t
	    (get-all-multipart-data request :limit *upload-file-size-limit*)))))
      (when file-size-limit-reached
	(error "upload file size limit exceeded"))
      (setf (aux-request-value 'bknr-parsed-body-parameters) parameters)
      (setf (aux-request-value 'uploaded-files) uploaded-files))))

(defun get-urlencoded-form-data ()
  (format t "get-urlencoded-form-data not ported~%")
  #+(or)
  (loop for name-value in (form-urlencoded-to-query (get-request-body))
	do (push name-value (aux-request-value 'bknr-parsed-body-parameters))))

(defun parse-request-body (&key uploads)
  (let ((content-type (header-in :content-type)))
    (cond
      ((null content-type)
       nil)
      ((scan #?r"^(?i)application/x-www-form-urlencoded" content-type)
       (format t "body parameters not parsed~%")
       #+(or)
       (get-urlencoded-form-data request))
      ((and uploads (scan #?r"^(?i)multipart/form-data" content-type))
       (format t "uploads not read~%")
       #+(or)
       (get-multipart-form-data)))))

(defun request-uploaded-files (&key all-info)
  "Return a list of conses (NAME . PATHNAME) which contains files uploaded by the user.
If :all-info is non-nil, the full upload file information is returned as a list"
  (format t "request-uploaded-files not yet ported~%")
  (if all-info
      (aux-request-value 'uploaded-files)
      (mapcar (lambda (upload) (cons (upload-name upload)
				     (upload-pathname upload)))
	      (aux-request-value 'uploaded-files))))

(defun request-uploaded-file (parameter-name)
  (cdr (find parameter-name (request-uploaded-files) :test #'equal :key #'car)))

(defun all-request-params ()
  "Return all non-empty request parameters - This includes all parameters encoded in the URL as
well as those in the request body, either as urlencoded strings or as multipart body.  If a multipart
body is present in the request, any uploaded files are saved in a temporary file and noted in the
request's plist.  Uploaded files will be automatically deleted by the with-http-response
macro after the request body has been executed."
  (unless (aux-request-value 'bknr-parsed-parameters)
    (let ((request-charset (or (register-groups-bind (charset) (#?r".*charset=\"?([^\"; ]+).*" (header-in :content-type)) charset)
			       "utf-8")))
      ;; request-charset is not currently used because there seems to
      ;; be no way to pass the character set for paramter decoding
      ;; down to Hunchentoot.  This will eventually be required (or
      ;; will it?)
      (setf (aux-request-value 'bknr-parsed-parameters)
	    (remove "" (query-params)
                    :key #'cdr :test #'string-equal))))
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
  (values-list (cddr (mapcar #'url-decode (split "/" (script-name))))))

(defun last-url-component ()
  (register-groups-bind (last)
      ("/([^\\/]+)$" (script-name))
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
    (subseq (script-name) len)))

(defun self-url (&key command prefix)
  (destructuring-bind
	(empty old-prefix object-id &rest old-command)
      (split "/" (script-name))
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
