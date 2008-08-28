(in-package :bknr.web)

(enable-interpol-syntax)

;; template expander

(defvar *template-expander*)
(defvar *template-env*)
(defparameter *template-dtd-catalog*
  (list (namestring (merge-pathnames #P"../../../../thirdparty/xhtml/catalog.xml" *load-pathname*))))

(eval-when (:load-toplevel :execute)
  (setf cxml:*catalog* (cxml:make-catalog *template-dtd-catalog*)
	cxml:*dtd-cache* (cxml:make-dtd-cache)
	cxml:*cache-all-dtds* t))

;; user-error is supposed to be raised when an error is provoked by
;; the user (i.e. by supplying invalid form data).

(define-condition user-error (simple-error)
  ())

(defun user-error (str &rest args)
  (error 'user-error :format-control str :format-arguments args))

;; template-not-found is raised when a template could not be found

(define-condition template-not-found (simple-error)
  ())

(defun template-not-found (&rest path)
  (error 'template-not-found :format-control "Template ~S not found" :format-arguments path))

(defclass template-expander ()
  ((command-packages :initarg :command-packages
		     :initform nil
		     :reader template-expander-command-packages)
   (destination :initarg :destination
		:reader template-expander-destination)
   (cached-templates :initform (make-hash-table :test 'equal)
                     :accessor template-expander-cached-templates)
   (default-template :initarg :default-template :initform nil
                     :reader template-expander-default-template
                     :documentation
                     "Name of the default template to use when no path
name has been specified.")))

(defmethod find-tag-function ((expander template-expander) name ns)
  (let ((package-name (cdr (find ns (template-expander-command-packages expander)
				 :test #'equal :key #'car)))
	(function-name (string-upcase name)))
    (or (gethash function-name (or (gethash (symbol-name package-name) *template-functions*)
				   (error "can't find package ~A in tag function registry" package-name)))
	(error "can't find tag function ~A:~A in command package ~A" ns name package-name))))

(defclass local-template-expander ()
  ((parent :initarg :parent :reader local-template-expander-parent)
   (functions :initform (make-hash-table :test #'equal) :reader local-template-expander-functions)))

(defmethod set-tag-function ((expander local-template-expander) name function)
  (setf (gethash name (local-template-expander-functions expander)) function))

(defmethod find-tag-function ((expander local-template-expander) name ns)
  (or (gethash name (local-template-expander-functions expander))
      (find-tag-function (local-template-expander-parent expander) name ns)))

(defmacro with-tag-expanders (tag-definitions &body body)
  `(let ((*template-expander* (make-instance 'local-template-expander :parent *template-expander*)))
     ,@(loop
	  for (name args body) in tag-definitions
	  collect `(set-tag-function *template-expander* (string-downcase (symbol-name ',name)) #'(lambda ,args ,body)))
     ,@body))

(defgeneric initial-template-environment (expander))

(defmethod initial-template-environment ((expander template-expander))
  (list* (mapcar #'(lambda (foo) (cons (make-keyword-from-string (car foo)) (cdr foo)))
                 (all-request-params))))

(defun get-template-var (var)
  (cdr (assoc var *template-env*)))

(defun (setf get-template-var) (newval var)
  (setf *template-env* (acons var newval *template-env*)))

;; xxx fixme setting variables has no effect on template-variables
(defmacro with-template-vars ((&rest vars) &body body)
  `(let (,@(loop for var in vars
                 collect
                   `(,var (get-template-var
                           ,(intern (symbol-name var) :keyword)))))
    ,@body))

(defun expand-variables (string lookup-variable)
  (if (find #\$ string)
      (regex-replace-all
       #?r"\$\(([\*_-\w]+)\)" string
       #'(lambda (target-string start end match-start match-end reg-starts reg-ends)
	   (declare (ignore start end match-start match-end))
	   (let* ((var (make-keyword-from-string
			(subseq target-string (aref reg-starts 0)
				(aref reg-ends 0))))
		  (val (funcall lookup-variable var)))
	     (cond
	       ((stringp val) val)
	       ((null val) "")
	       (t (format nil "~A" val))))))
      string))

(defun xmls-attributes-to-sax (fn attrs)
  (mapcar (lambda (a)
            (destructuring-bind (name value) a
	      (if (listp name)
		  (destructuring-bind (qname . namespace-uri) name
		    (sax:make-attribute :namespace-uri namespace-uri
					:qname qname
					:value (funcall fn value)
					:specified-p t))
		  (sax:make-attribute :qname name
				      :value (funcall fn value)
				      :specified-p t))))
	  attrs))

(defun parse-template (template-pathname)
  (let ((sax:*include-xmlns-attributes* t))
    (cxml:parse-file (namestring (probe-file template-pathname))
		     (cxml-xmls:make-xmls-builder)
                     :validate nil)))

(defvar *tag-children*)

(defun emit-tag-children ()
  "Function to be called by application defined tags to emit their children."
  (mapc (curry #'emit-template-node *template-expander*) *tag-children*))

(defun emit-template-node (expander node)
  (if (stringp node)
      (sax:characters *html-sink* (expand-variables node #'get-template-var))
      (let* ((name (node-name node))
             (ns (node-ns node))
             (children (node-children node))
             (attrs (cxml-xmls:node-attrs node))
             (ns-handler-package (and ns (find ns (template-expander-command-packages expander)
                                               :test #'equal :key #'car))))
        (cond
          (ns-handler-package
	   (let ((*tag-children* children))
	     (apply (find-tag-function expander name ns)
		    (append (loop for (key name) in attrs
                                  unless (consp key) ; ignore attributes with namespace
				  collect (make-keyword-from-string key)
                                  and collect (expand-variables name #'get-template-var))))))
          (t
           (sax:start-element *html-sink* nil nil name
                              (xmls-attributes-to-sax (rcurry #'expand-variables #'get-template-var) attrs))
           (dolist (child children)
             (emit-template-node expander child))
           (sax:end-element *html-sink* nil nil name))))))

(defun emit-parsed-template (expander toplevel)
  "Emit the given XMLS compatible structure as XML to *HTML-SINK*."
  ;; In order to generate xmlns attributes, we use the internal
  ;; CXML-XMLS::COMPUTE-ATTRIBUTES/LNAMES function.  This may need to
  ;; be revised with newer cxml releases.
  (sax:start-element *html-sink* (node-ns toplevel) (node-name toplevel) (node-name toplevel)
		     (cxml-xmls::compute-attributes/lnames toplevel t))
  (let ((*template-expander* expander))
    (dolist (node (node-children toplevel))
      (emit-template-node expander node)))
  (sax:end-element *html-sink* (node-ns toplevel) (node-name toplevel) (node-name toplevel)))

(defun find-template (dir components)
  (if (null components)
      nil
      (let ((next-dir (merge-pathnames (make-pathname :directory (list :relative (first components)))
				       dir)))
	(when (probe-file next-dir)
	  (let ((result (multiple-value-list (find-template next-dir (cdr components)))))
	    (when (car result)
	      (return-from find-template (values-list result)))))
	(let ((file (merge-pathnames (make-pathname :type "xml"
						    :name (first components))
				     dir)))
	  (when (probe-file file)
	    (values file (cdr components)))))))

(defun split-path (path)
  "Split path into its components and return them as list.  Empty components are removed."
  (remove "" (split "/" path) :test #'equal))

(defmethod find-template-pathname ((expander template-expander) template-name)
  (let ((components (or (split-path template-name)
                        (and (template-expander-default-template expander)
                             (split-path (template-expander-default-template expander))))))
    (multiple-value-bind (pathname ret-components)
	(find-template (template-expander-destination expander) components)
      (unless pathname
	(template-not-found template-name))
      (values pathname
	      ret-components
	      (with-output-to-string (s)
		(dolist (component (subseq components 0 (- (length components)
							   (length ret-components))))
		  (write-char #\/ s)
		  (write-string component s)))))))

(defun get-cached-template (pathname expander)
  (let* ((table (template-expander-cached-templates expander))
         (namestring (namestring pathname))
         (cache-entry (gethash namestring table))
         (current-write-date (file-write-date namestring)))
    (unless (and cache-entry (eql (car cache-entry) current-write-date))
      (setf cache-entry
            (cons current-write-date (parse-template pathname)))
      (setf (gethash namestring table) cache-entry))
    (cdr cache-entry)))

(defun emit-template (expander stream node env)
  (let* ((*template-env* env)
         (*html-sink* (cxml:make-character-stream-sink stream :canonical nil)))
    (if (node-attribute node "suppress-xml-headers")
	(emit-parsed-template expander node)
	(progn
	  (sax:start-document *html-sink*)
	  (sax:start-dtd *html-sink*
			 "html"
			 "-//W3C//DTD XHTML 1.0 Transitional//EN"
			 "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd")
	  (sax:end-dtd *html-sink*)
	  (emit-parsed-template expander node)))
    (sax:end-document *html-sink*)))
;; template handler

(defclass template-handler (prefix-handler template-expander)
  ())

(defmethod expand-template ((handler template-handler)
			    template-name &key env)
  (multiple-value-bind (template-pathname args template-path)
      (find-template-pathname handler template-name)
    (unless template-pathname
      (return-from expand-template nil))
    (unless (request-variable :template-args)
      (setf (request-variable :template-path) template-path)
      (setf (request-variable :template-args) args))
    (let ((i 0))
      (dolist (arg args)
	(setf env
	      (acons (make-keyword-from-string (format nil "*path-arg-~a*" i))
		     arg
		     env))
	(incf i)))
    (setf env (acons :*path-arg* (first args) env))
    (if (probe-file template-pathname)
        ;; Wir koennten hier direkt auf *html-stream* schreiben ohne
        ;; den Umweg ueber einen String zu gehen.  Aber: Wir moechten
        ;; im Allgemeinen erst waehrend des Expandierens noch merken
        ;; koennen, dass z.B. ein Fehler vorliegt oder ein Redirect
        ;; geschickt werden muss.  Daher waere es keine gute Idee, sich
        ;; zu diesem Zeitpunkt schon auf einen HTTP response code
        ;; festgelegt zu haben.
	(let ((*default-pathname-defaults* (make-pathname :host (pathname-host template-pathname)
							  :device (pathname-device template-pathname)
							  :directory (pathname-directory template-pathname))))
	  (with-output-to-string (stream)
	    (emit-template handler
			   stream
			   (get-cached-template template-pathname handler)
			   env)))
        (template-not-found template-pathname))))

(defmethod error-template-pathname (handler &optional (error-type "user-error"))
  (find-template-pathname handler error-type))

(defun send-error-response (handler message &key (response-code +http-internal-server-error+))
  (with-http-response (:content-type "text/html; charset=UTF-8"
				     :response response-code)
    (with-output-to-string (stream)
      (emit-template handler
		     stream
		     (get-cached-template (error-template-pathname handler) handler)
		     (acons :error-message message
			    (initial-template-environment
			     handler))))))

(defun invoke-with-error-handlers (fn handler)
  (handler-case
      (funcall fn)
    (user-error (c)
      (send-error-response handler (apply #'format
                                          nil
                                          (simple-condition-format-control c)
                                          (simple-condition-format-arguments c))
                           :response-code +http-ok+))
    (template-not-found (c)
      (send-error-response handler (apply #'format
                                          nil
                                          (simple-condition-format-control c)
                                          (simple-condition-format-arguments c))
                           :response-code +http-not-found+))
    (serious-condition (c)
      (warn "unexpected failure: ~A" c)
      (send-error-response handler (format nil "Internal Error:~%~%~A~%" c)))))

(defmacro with-error-handlers ((handler) &body body)
  `(invoke-with-error-handlers (lambda () ,@body) ,handler))

(defmethod handler-matches-p ((handler template-handler))
  (handler-case 
      (find-template-pathname handler (subseq (script-name*) 1))
    (template-not-found (c)
      (declare (ignore c))
      nil)))

(defmethod handle ((handler template-handler))
  (with-error-handlers (handler)
    ;; Erst body ausfuehren...
    (let ((body
           (expand-template handler
			    (subseq (script-name*)
				    (length (page-handler-prefix handler)))
                            :env (initial-template-environment handler))))
      ;; ... und wenn keine Fehler entdeckt wurden, rausschreiben
      (if body
          (with-http-response (:content-type "text/html; charset=UTF-8"
					     :response +http-ok+)
            body)
          (error-404)))))

;; XXX documentation-handler sieht interessant aus, unbedingt reparieren
(defclass documentation-handler (page-handler)
  ((package :initarg :package
	    :reader documentation-handler-package)
   (name :initarg :name
	 :initform :documentation)))

#+(or)
(defmethod handle ((page-handler documentation-handler))
  (let ((symbol-docs (sort (loop for sym being the external-symbols
				 in (documentation-handler-package page-handler)
				 for documentation = (documentation sym 'function)
				 when documentation
				 collect (cons (concatenate 'string "eboy:"
							    (string-downcase (symbol-name sym)))
					       documentation))
			   #'string<= :key #'car)))
    (with-eboy-page (:title "Template documentation" )
      (html "This page documents the available template processing
tags.  These tags can be used in .bknr files and will be dynamically
expanded at page generation time.  New tags can be defined by writing
Common Lisp functions."
	    (:h2 "Index:")
	    ((:div :class "index")
	     (:ul
	      (loop for (symbol . documentation) in symbol-docs
		    do (html (:li (cmslink #?"#${symbol}" (:princ-safe symbol)))))))
	    (:h2 "Function documentation:")
	    (loop for (symbol . documentation) in symbol-docs
		  do (html ((:div :class "function")
			    (cmslink symbol #?"${symbol}")
			    (:h3 (:princ-safe symbol))
			    (:pre (:princ-safe documentation)))))))))

