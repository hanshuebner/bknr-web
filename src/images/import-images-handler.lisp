(in-package :bknr.images)

(enable-interpol-syntax)

(defclass image-import-handler (import-handler)
  ())

(defmethod import-handler-spool-files ((handler image-import-handler))
  (image-directory-recursive (import-handler-import-pathname handler)))

(defmethod handle-form ((handler image-import-handler) action)
  (with-bknr-page (:title #?"image import directory")
    ((:form :method "post")
     ((:div :class "keyword-choose")
      (if (class-subclasses 'store-image)
	(html
	 (:h2 "Choose class for the imported images:")
	 (select-box "class-name" (mapcar #'(lambda (class-name) (format nil "~S" class-name)) (cons 'store-image (class-subclasses 'store-image)))
		     :default (format nil "~S" 'store-image)))
	(html
	 ((:input :type "hidden" :name "class-name" :value "BKNR.IMAGES:STORE-IMAGE"))))
      (:h2 "Choose keywords for the imported images:")
      (image-keyword-choose-dialog :create t :size "4")
      (:div ((:input :type "checkbox" :name "keyfromdir" :value "on" :checked "checked")
	     "Set keywords from directory"))
      (:div (submit-button "import" "Import"))))
    ((:div :class "import-list")
     (:h2 "Images present in import spool:")
     (loop for file in (import-handler-spool-files handler)
	   do (html (:princ-safe (namestring file)) (:br))))))

(defmethod import-handler-import-files ((handler image-import-handler))
  (let* ((keywords (keywords-from-query-param-list (query-param-list "keyword")))
	 (spool-dir (import-handler-import-pathname handler))
	 (class-name (apply #'find-symbol (reverse (split "::?" (query-param "class-name"))))))
    (import-directory spool-dir
		      :class-name class-name
		      :user (bknr-session-user)
		      :keywords keywords
		      :spool (import-handler-spool-dir handler)
		      :keywords-from-dir (query-param "keyfromdir"))))

(defmethod handle-form ((handler image-import-handler) (action (eql :import)))
  (let* ((import-log (import-handler-import-files handler))
	 (successful-images (remove-if-not #'(lambda (element) (typep element 'store-image))
					   import-log
					   :key #'cdr))
	 (error-log (remove-if-not #'(lambda (element) (typep element 'error))
				   import-log
				   :key #'cdr)))
    (with-bknr-page (:title #?"bknr import log")
      ((:div :class "error-log") (:h2 "Errors during import:")
       (loop for (file . error) in error-log
	     do (typecase error
		  (index-existing-error
		   (html "Error importing " (:princ-safe file)
			 " because it already is present in the system" (:br)))
		  (t
		   (html "Error importing " (:princ-safe file)
			 (:princ-safe (format nil ": ~a" error)) (:br))))))
      (bknr.sysparams:set-sysparam :last-modified (get-universal-time))
      (image-collection (mapcar #'cdr successful-images)
				       :title "Successfully imported images:"))))
