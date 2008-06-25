(in-package :bknr.web)

(enable-interpol-syntax)

(defvar *bknr-debug* nil)
(defvar *website* nil)

(defvar *website-modules* (make-hash-table :test #'equal))

(defmacro define-bknr-webserver-module (name &rest handler-definitions)
  `(setf (gethash (symbol-name ',name) *website-modules*) ',handler-definitions))

(defstruct choice title link submenu)

(defclass website ()
  ((name :initarg :name
	 :reader website-name)
   (url :initarg :url
	:accessor website-url)
   (authorizer :initarg :authorizer
	       :accessor website-authorizer)
   (handler-definitions :initarg :handler-definitions
			:accessor website-handler-definitions)
   (handlers :initform nil :accessor website-handlers)
   (cachable-handlers :initform nil :accessor website-cachable-handlers)
   (menu :initarg :menu)
   (menudef-xml-file :initarg :menudef-xml-file
		     :accessor website-menudef-xml-file)
   (menudef-last-read :initform nil)
   (navigation :initarg :navigation
	       :accessor website-navigation)
   (admin-navigation :initarg :admin-navigation
		     :accessor website-admin-navigation)
   (base-href :initarg :base-href
              :accessor website-base-href
              :initform "/")
   (style-sheet-urls :initarg :style-sheet-urls
		     :accessor website-style-sheet-urls)
   (javascript-urls :initarg :javascript-urls
		    :accessor website-javascript-urls)
   (site-logo-url :initarg :site-logo-url
		  :accessor website-site-logo-url)
   (login-logo-url :initarg :login-logo-url
		   :accessor website-login-logo-url)
   (rss-feed-url :initarg :rss-feed-url
		 :accessor website-rss-feed-url)
   (import-spool-directory :initarg :import-spool-directory
			   :accessor website-import-spool-directory)
   (template-base-directory :initarg :template-base-directory
                            :reader website-template-base-directory)
   (template-command-packages :initarg :template-command-packages
                              :reader website-template-command-packages)
   (template-handler :initform nil
                     :reader website-template-handler))
  (:default-initargs :url nil
    :authorizer (make-instance 'bknr-authorizer)
    :menu nil
    :navigation nil
    :admin-navigation nil
    :style-sheet-urls nil
    :javascript-urls nil
    :login-logo-url "/image/bknr-logo"
    :site-logo-url "/image/bknr-logo"
    :import-spool-directory #p"/home/bknr/spool/"
    :template-base-directory nil
    :template-command-packages nil
    :rss-feed-url nil)
  (:documentation "Class to hold all information on a web server that
is served within BKNR.  Currently, this is a singleton object, and
*WEBSITE* will point to the only instance."))

(defmethod initialize-instance :after ((website website) &key &allow-other-keys)
  (when *website*
    (warn "Warning, *website* redefined with new website definition"))
  (setf *website* website)
  (publish-site *website*))

(defun website-host ()
  (if (and (boundp 'hunchentoot::*request*)
           hunchentoot::*request*
           (hunchentoot:header-in* :host))
      (header-in* :host)
      "localhost"))

(defmethod show-handlers ((website website))
  (dolist (handler (website-handlers website))
    (format t "~A => ~A~%" (uri-path (page-handler-url handler)) handler)))

(defun relative (path)
  (if (eq #\/ (aref path 0))
      (relative (subseq path 1))
      path))

(defmethod website-make-path ((website website) path)
  (format nil "~A~A" (website-base-href website) (relative path)))

(defgeneric publish-handler (website handler)
  (:documentation "Publish HANDLER on WEBSITE, thereby adding it to
the chain of handlers that is searched to handle an incoming
request."))

(defgeneric handler-matches-p (handler)
  (:documentation "Determine whether HANDLER is willing to handle the
current request.  Returns non-NIL if the HANDLER wants to handle the request
NIL otherwise."))

(defgeneric publish-site (website)
  (:documentation "Publish all handlers defined in WEBSITE.

XXX When is this called?"))

(defun handler-definition-name (handler-definition)
  (first handler-definition))

(defun handler-definition-class (handler-definition)
  (second handler-definition))

(defun handler-definition-initargs (handler-definition)
  (nthcdr 2 handler-definition))

(defmethod website-handler-with-name ((website website) name)
  (find name (website-handlers website) :key #'page-handler-name))

(defun parse-choice (choice-xml)
  (let ((choice (make-choice)))
    (dolist (attr-value choice-xml)
      (destructuring-bind (attr value) attr-value
	(ecase (make-keyword-from-string attr)
	  (:link (setf (choice-link choice) value))
	  (:title (setf (choice-title choice) value)))))
    (unless (choice-link choice)
      (setf (choice-link choice) (choice-title choice)))
    choice))

(defun process-choices-xml (choices-xml)
  (let (choices)
    (dolist (choice-xml choices-xml)
      (push (parse-choice (cadr choice-xml)) choices)
      (when (cddr choice-xml)
	(setf (choice-submenu (first choices)) (process-choices-xml (cddr choice-xml)))))
    (reverse choices)))

(defgeneric process-handler-definition (website definition)
  (:documentation "Process a handler definition entry DEFINITION which
may either be a LIST of (PATH HANDLER-CLASS &optional INITARGS) or a
symbol, denoting a module to load at this point in the (linear)
handler definition.  Every method returns a list of handler instances.")
  (:method (website (definition list))
    (list (apply #'make-instance (handler-definition-class definition)
		 :name (handler-definition-name definition)
		 :site website
		 (handler-definition-initargs definition))))
  (:method (website (module-name symbol))
    (mapcan (curry #'process-handler-definition website)
	    (or (gethash (symbol-name module-name) *website-modules*)
		(error "bknr module ~A not known" module-name)))))

(defmethod publish-site ((website website))
  (setf (website-handlers website)
	(mapcan (curry #'process-handler-definition website)
		(website-handler-definitions website)))
  ;; XXX implicitly creating a template handler seems wrong:
  (when (website-template-base-directory website)
    (setf (slot-value website 'template-handler) (make-instance 'template-handler
                                                                :name "/"
                                                                :site website
                                                                :destination (website-template-base-directory website)
                                                                :command-packages (website-template-command-packages website)))
    (push (website-template-handler website)
          (website-handlers website)))
  (mapc (curry #'publish-handler website) (website-handlers website))
  (pushnew 'bknr-dispatch *dispatch-table*))

(defmethod website-session-info ((website website))
  (html ((:div :id "session-info")
	 "local time is " (:princ-safe (format-date-time))
	 (if (bknr-session-user)
	     (html ", logged in as " (html-link (bknr-session-user)))
	     (html ", not logged in")))))


(defclass cachable-handler ()
  ())

(defmethod initialize-instance :after ((handler cachable-handler) &rest initargs)
  (declare (ignore initargs))
  (push handler (website-cachable-handlers (page-handler-site handler))))

(defclass page-handler ()
  ((prefix :initarg :prefix
	   :accessor page-handler-prefix)
   (name :initarg :name
	 :reader page-handler-name)
   (title :initarg :title
	  :reader page-handler-title
	  :documentation "textual title for the page used in menus"
	  :initform nil)
   (function :initarg :function
	     :reader page-handler-function)
   (require-user-flag :initarg :require-user-flag
		      :reader page-handler-require-user-flag
		      :initform nil)
   (content-type :initarg :content-type
		 :reader page-handler-content-type
		 :initform "text/html")
   (site :initarg :site
	 :reader page-handler-site))
  (:documentation "Simple page handler publishing a serve request under a simple URL"))

(defmethod initialize-instance :after ((handler page-handler) &key name prefix &allow-other-keys)
  (unless prefix
    (setf (page-handler-prefix handler)
	  (if (stringp name)
	      name
	      (concatenate 'string "/" (string-downcase name))))))

(defmethod print-object ((handler page-handler) stream)
  (print-unreadable-object (handler stream :type t)
    (format stream "~A" (page-handler-prefix handler))))

(defgeneric handle (page-handler)
  (:documentation "Handle an incoming HTTP request, returning either a
string or an (array (unsigned-byte 8) (*)) with the response
contents.  Alternatively, the handler may call (SEND-HEADERS) to
get access to the response stream and output the data to it."))

(defgeneric authorized-p (page-handler)
  (:documentation "Return non-nil if the request is authorized to be
executed on PAGE-HANDLER

XXX wouldn't it be better if handler-matches-p checked
authorization?"))

(defgeneric page-handler-url (page-handler)
  (:documentation "Return the full base URL for PAGE-HANDLER."))

(defmethod handler-path ((handler page-handler))
  (subseq (script-name*)
	  (length (page-handler-prefix handler))))

(defmethod decoded-handler-path ((handler page-handler))
  (mapcar #'url-decode
	  (remove ""
		  (split "/" (handler-path handler))
		  :test #'equal)))

(defmethod parse-handler-url ((handler page-handler))
  (values-list (decoded-handler-path handler)))

(defmethod page-handler-url ((handler page-handler))
  (merge-uris (parse-uri (page-handler-prefix handler))
	      (website-url (page-handler-site handler))))

(defmethod authorized-p ((page-handler page-handler))
  (with-slots (require-user-flag) page-handler
    (if (and require-user-flag
	     (not (find require-user-flag
			(user-flags (bknr-session-user)))))
	nil
	t)))

(defmethod invoke-handler ((handler page-handler))
  (let* ((*website* (page-handler-site handler))
	 (*req-var-hash* (or *req-var-hash*
			     (make-hash-table)))
         (*random-state* (make-random-state t)))
    (do-log-request)
    (handler-bind
        ((error #'(lambda (e)
                    (with-http-response (:content-type "text/html; charset=UTF-8"
                                                       :response +http-internal-server-error+)
                      (return-from invoke-handler (prog1
                                                      (with-http-body ()
                                                        (website-show-error-page *website* e))
                                                    (do-error-log-request e)))))))
      (handle handler))))

(defmethod handle ((page-handler page-handler))
  (funcall (page-handler-function page-handler)))

(defvar *handlers* nil)

(defun ensure-bknr-session ()
  "Ensure that the BKNR-SESSION session variable is set and that it
belongs to the user that is specified in the request."
  (start-session)
  (let ((request-user (authorize (website-authorizer *website*))))
    (when (or (not (session-value 'bknr-session))
              (and request-user
                   (not (eq (bknr-session-user) request-user))))
      (setf (session-value 'bknr-session)
            (make-instance 'bknr-session :user (or request-user
                                                   (find-user "anonymous")
                                                   (error "cannot find \"anonymous\" user"))))))
  (session-value 'bknr-session))

(defun bknr-dispatch (request)
  (declare (ignore request))
  (let ((handler (find-if #'handler-matches-p (website-handlers *website*))))
    (cond
      (handler
       (cond
         ((authorized-p handler)
          (curry #'invoke-handler handler))
         (t
          (setf (session-value :login-redirect-uri)
                (redirect-uri (parse-uri (script-name*))))
          (redirect (website-make-path *website* "login")))))
      (t
       'error-404))))

(defmethod publish-handler ((website website) (handler page-handler))
  (setf *handlers* (append *handlers* (list handler))))

(defmethod handler-matches-p ((handler page-handler))
  (string-equal (page-handler-prefix handler)
		(script-name*)))

(defclass redirect-handler (page-handler)
  ((to :initarg :to :reader redirect-handler-to :documentation "url to redirect to")))

(defmethod initialize-instance :after ((handler redirect-handler) &key to)
  (assert (equal #\/ (aref to 0))
          () "path ~S provided as target to redirect-handler does not begin with a slash"
          to))

(defmethod handle ((page-handler redirect-handler))
  (redirect (redirect-handler-to page-handler)))

(defclass random-redirect-handler (redirect-handler)
  ())

(defmethod handle ((page-handler random-redirect-handler))
  (redirect (random-elt (redirect-handler-to page-handler))))

(defclass form-handler (page-handler)
  ()
  (:documentation "A FORM-HANDLER is a handler that processes form
submissions.  The handler generic function for FORM-HANDLER subclasses
is called HANDLE-FORM."))

(define-condition form-condition (condition)
  ((reason :initarg :reason
	   :reader form-condition-reason
	   :initform nil)))

(define-condition form-field-missing-condition (form-condition)
  ((field :initarg :field
	  :reader form-field-missing-condition-field)))

(define-condition form-not-authorized-condition (form-condition)
  ())

(defmacro ensure-form-field (field-name)
  `(unless ,field-name
    (signal (make-condition 'form-field-missing-condition
	     :field ',field-name))))

(defgeneric handle-form (page-handler action)
  (:documentation "Handle form submission for PAGE-HANDLER.  The
form variable \"action\" will be parsed into a keyword and passwd to
the invocation of this generic function as ACTION.  Methods are meant
to specialize on individual keywords to handle different actions that
the form supports."))

(defmethod handle ((page-handler form-handler))
  (let* ((form (query-param "action"))
	 (form-keyword (when form (make-keyword-from-string form))))
    (handle-form page-handler form-keyword)))

(defclass prefix-handler (page-handler)
  ()
  (:documentation "A PREFIX-HANDLER is a handler that is invoked for
URLs with a certain prefix, as determined by the :PREFIX
initialization argument.  It is used as a mixin class and only
provides for a HANDLER-MATCHES-P method."))

#+(or)
(defmethod initialize-instance :after ((handler prefix-handler) &key)
  (unless (eql #\/ (aref (page-handler-prefix handler)
                         (1- (length (page-handler-prefix handler)))))
    (warn "prefix handler ~A does not have prefix ending with / - may match unexpectedly" handler)))

(defmethod handler-matches-p ((handler prefix-handler))
  (and (>= (length (script-name*))
	   (length (page-handler-prefix handler)))
       (string-equal (page-handler-prefix handler)
		     (script-name*)
		     :end2 (length (page-handler-prefix handler)))))

(defclass directory-handler (prefix-handler)
  ((destination :initarg :destination
		:reader page-handler-destination))
  (:documentation
   "Handler for a directory in the file system.  Publishes all files
in the directory DESTINATION under their relative path name."))

(defgeneric request-relative-pathname (directory-handler)
  (:documentation "Return the relative pathname for the current
request as determined by DIRECTORY-HANDLER.")
  (:method ((handler directory-handler))
    (or (aux-request-value 'request-relative-pathname)
        (setf (aux-request-value 'request-relative-pathname)
              (pathname (subseq (script-name*) (1+ (length (page-handler-prefix handler)))))))))

(defmethod handler-matches-p ((handler directory-handler))
  (and (call-next-method)
       (probe-file (merge-pathnames (request-relative-pathname handler)
				    (page-handler-destination handler)))))

(defmethod handle ((handler directory-handler))
  (handle-static-file (merge-pathnames (request-relative-pathname handler)
				       (page-handler-destination handler))))

(defclass file-handler (page-handler)
  ((destination :initarg :destination
		:reader page-handler-destination)
   (content-type :initarg :content-type
		 :reader page-handler-content-type))
  (:default-initargs :content-type "text/plain")
  (:documentation "A FILE-HANDLER is used to publish a single file
under an URL.  :DESTINATION is the pathname of the file to publish,
:CONTENT-TYPE is the content type to use."))

(defmethod handle ((handler file-handler))
  (handle-static-file (page-handler-destination handler) (page-handler-content-type handler)))

(defclass object-handler (prefix-handler)
  ((query-function :initarg :query-function :reader object-handler-query-function)
   (object-class :initarg :object-class :reader object-handler-object-class))
  (:default-initargs :object-class t :query-function nil)
  (:documentation "An OBJECT-HANDLER handles requests for a persistent
object in the BKNR datastore.  The first component of the relative
path of the request is used as the key to locate the object.  The
object is looked up using the OBJECT-HANDLER-GET-OBJECT generic
function, which will, by default, call FIND-STORE-OBJECT with the key
as argument.

The :QUERY-FUNCTION initarg may be supplied to set up a query function
that maps from the key to an object.  It is typically used to
implement lookup by name.

The :OBJECT-CLASS initarg may be supplied to restrict the handler to
operate on objects of that class or its subclass.  If the key
specifies the ID of an object that has a different class or if the
QUERY-FUNCTION returns an object of a different class, a run-time
error is signaled."))

(defgeneric object-handler-get-object (object-handler)
  (:documentation "Implement object lookup.  Methods return the object
that the current request should operate upon.  The default method for
the OBJECT handler class performs a lookup using FIND-STORE-OBJECT.")
  (:method ((handler object-handler))
    (let ((id (parse-url)))
      (when id
        (find-store-object id
                           :class (object-handler-object-class handler)
                           :query-function (object-handler-query-function handler))))))

(defgeneric handle-object (object-handler object)
  (:documentation "Handle the current to the OBJECT-HANDLER.  OBJECT
is the object as looked up by OBJECT-HANDLER-GET-OBJECT."))

(defmethod handle ((handler object-handler))
  (let ((object (object-handler-get-object handler)))
    (handle-object handler object)))

(defclass edit-object-handler (form-handler object-handler)
  ()
  (:documentation "Combined FORM-HANDLER and OBJECT-HANDLER class that
is used as a base class for handlers that edit an object using a HTML
form."))

(defgeneric handle-object-form (handler action object)
  (:documentation "Perform ACTION, a keyword that has been determined by
parsing the \"action\" input element of the current HTML form, on
OBJECT, which is parsed using the mechanism of an OBJECT-HANDLER."))

(defmethod handle-form ((handler edit-object-handler) action)
  (let ((object (object-handler-get-object handler)))
    (handle-object-form handler action object)))

(defmethod handle-object-form ((handler edit-object-handler) action (object (eql nil)))
  (with-bknr-page (:title "No such object")
    (html "No such object, ieeeh")))

(defclass keyword-handler (object-handler)
  ())

(defmethod object-handler-get-object ((handler keyword-handler))
  (let ((keystr (parse-url)))
    (when keystr
      (make-keyword-from-string keystr))))

(defclass keywords-handler (object-handler)
  ())

(defmethod object-handler-get-object ((handler keywords-handler))
  (let ((keystr (parse-url)))
    (if keystr
	(mapcar #'(lambda (k) (make-keyword-from-string (string-upcase k)))
		(split "," keystr))
	nil)))

(defclass object-list-handler (object-handler)
  ())

(defgeneric object-list-handler-get-objects (handler object))
(defgeneric object-list-handler-title (handler object))
(defgeneric object-list-handler-rss-link (handler object))

(defclass object-date-list-handler (object-list-handler)
  ())

(defgeneric object-date-list-handler-grouped-objects (handler object))

(defgeneric object-date-list-handler-date (handler object)
  (:method ((handler object-date-list-handler) object)
    (with-query-params (date)
      (get-daytime (if date
                       (or (parse-integer date :junk-allowed t)
                           (get-universal-time))
                       (get-universal-time))))))

(defclass admin-only-handler ()
  ())

(defmethod authorized-p ((handler admin-only-handler))
  (admin-p (bknr-session-user)))

(defclass xml-handler ()
  ((style-path :initarg :style-path :reader xml-handler-style-path))
  (:default-initargs :style-path nil))

(defmethod handle :around ((handler xml-handler))
  (with-http-response (:content-type "text/xml")
    (with-output-to-string (stream)
      (let ((sink (cxml:make-character-stream-sink stream :canonical t))
	    (style-path (or (query-param "style")
			    (xml-handler-style-path handler))))
	(cxml:with-xml-output sink
	  (when style-path
	    (sax:processing-instruction sink
					(runes:string-rod "xml-stylesheet")
					(runes:string-rod (format nil "type=\"text/xsl\" href=\"~A\""
								  style-path))))
	  (call-next-method))))))

(defclass xml-object-handler (object-handler xml-handler)
  ())

(defmethod handle-object ((handler xml-object-handler) (object (eql nil)))
  (error "invalid object id"))

(defgeneric xml-object-handler-show-object (handler object))

(defmethod xml-object-handler-show-object ((handler xml-object-handler) object)
  (write-to-xml object))

(defmethod handle-object ((handler xml-object-handler) object)
  (xml-object-handler-show-object handler object))

(defclass xml-object-list-handler (object-handler xml-handler)
  ((toplevel-element-name :initarg :toplevel-element-name :reader xml-object-list-handler-toplevel-element-name))
  (:default-initargs :toplevel-element-name "objects"))

(defmethod object-handler-get-object ((handler xml-object-list-handler))
  (multiple-value-list (parse-url)))

(defgeneric object-list-handler-show-object-xml (handler object))

(defmethod object-list-handler-show-object-xml ((handler xml-object-list-handler) object)
  #+(or) (set-string-rod-fn #'cxml::utf8-string-to-rod)
  (write-to-xml object))

(defmethod handle-object ((handler xml-object-list-handler) object)
  (let ((element-name (xml-object-list-handler-toplevel-element-name handler)))
    (cxml:with-element element-name
      (dolist (object (object-list-handler-get-objects handler object))
	(object-list-handler-show-object-xml handler object)))))

(defclass blob-handler (object-handler)
  ())

(defmethod handle-object ((handler blob-handler) (blob blob))
  (with-http-response (:content-type (blob-mime-type blob))
    (setf (content-length) (blob-size blob))
    (let ((stream (send-headers)))
      (blob-to-stream blob stream))))

(defclass import-handler (form-handler)
  ((require-user-flag :initform :admin)
   (spool-dir :initarg :spool-dir
	      :initform *user-spool-directory-root*
	      :reader import-handler-spool-dir)))

(defgeneric import-handler-import-pathname (handler))
(defgeneric import-handler-spool-files (handler))
(defgeneric import-handler-import-files (handler))

(defmethod import-handler-import-pathname ((handler import-handler))
  (let* ((user (bknr-session-user))
	 (spool-dir (merge-pathnames (make-pathname
				      :directory (list :relative (user-login user)))
				     (import-handler-spool-dir handler))))
    (ensure-directories-exist spool-dir)
    spool-dir))

(defmethod website-show-page ((website website) fn title)  
  (html   
   (:html
    (:head
     (header :title title))
    ((:body :class "cms")
     ((:div :class "navigation")
      (logo)
      (:h1 (:princ-safe (website-name website)))
      (navigation))
     (when title
       (html (:h1 (:princ-safe title))))
     (funcall fn)
     (session-info)))))

(defmethod website-show-error-page ((website website) error)
  (if (and (website-template-handler website)
           (error-template-pathname (website-template-handler website)))
      (send-error-response (website-template-handler website) (princ-to-string error))
      (html
       (:html
        (:head
         (header :title "Error processing your request"))
        ((:body :class "cms")
         (:h1 "Error processing your request")
         (:p "While processing your request, an error occured:")
         ((:div :class "error")
	  (:princ-safe error)))))))

(defun show-page-with-error-handlers (fn &key (response +http-ok+) title)
  (setf (return-code) response)
  (with-http-response (:content-type "text/html; charset=UTF-8" :response response)
    (with-http-body ()
      (website-show-page *website* fn title))))

(defmacro with-bknr-page ((&rest args) &body body)
  `(show-page-with-error-handlers (lambda () (html ,@body)) ,@args))

#+(or)
(defmacro with-bknr-site-template ((&key title) &rest body)
  `((with-http-response (:content-type "text/html")
    (with-http-body ()
      (include
       :template "toplevel-template"
       :tag-body (with-output-to-string (*html-stream*)
		   ,@body))))))

(defun unpublish ()
  (setf *dispatch-table* (remove 'bknr-dispatch *dispatch-table*)
	*handlers* nil))
