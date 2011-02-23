(in-package :bknr-user)

(defclass class-browser-handler (object-handler)
  ((default-package-name :initarg :default-package-name))
  (:default-initargs :default-package-name nil))
   
(defmethod object-handler-get-object ((handler class-browser-handler))
  (destructuring-bind (class-name &optional (package-name (slot-value handler 'default-package-name)))
      (mapcar #'string-upcase (reverse (split "::" (parse-url))))
    (find-class (find-symbol class-name (find-package package-name)) nil)))

(defmethod handle-object ((handler class-browser-handler) (class (eql nil)))
  (user-error "Invalid class name ~A" (parse-url)))

(defmethod handle-object ((handler class-browser-handler) class)
  (with-http-response ()
    (with-http-body ()
	(labels ((qualified-class-name (class)
		   (let ((class-name (class-name class)))
		     (format nil "~A::~A" (package-name (symbol-package class-name)) (symbol-name class-name))))
		 (show-class (class &optional (level 0))
		   (html
		    ((:div :class "class" :style (format nil "background-color: ~A;" (if (evenp level) "#ffffff" "#e0e0e0")))
		     (:h1 ((:a :href (format nil "~A/~A" (page-handler-prefix handler) (qualified-class-name class)))
			   (:princ-safe (qualified-class-name class))))
		     (when (documentation class t)
		       (html
			(:p (:princ-safe (documentation class t)))))
		     (dolist (subclass (closer-mop:class-direct-subclasses class))
		       (show-class subclass (1+ level)))))))
	  (html
	   (:head
	    (:title "Class browser for class" class)
            #+(or)
	    (js:css (body :font-family "sans-serif"
			  :font-size "10px")
		    (h1 :font-size "130%")
		    (div.class :padding "5px"
			       :margin-left "10px"
			       :margin-top "4px"
			       :border "thin solid #000000"
			       :-moz-border-radius "8px")))
	   (:body
	    (show-class class)))))))