(in-package :bknr.images)

(enable-interpol-syntax)

(defclass edit-images-handler (edit-object-handler image-handler)
  ((require-user-flag :initform :admin)))

(defmethod object-handler-get-object ((handler edit-images-handler))
  (remove nil (mapcar #'(lambda (id)
			  (find-store-object
			   id :class 'store-image
			   :query-function #'store-image-with-name))
		      (query-param-list "image-id"))))

(defmethod handle-object-form ((handler edit-images-handler) action images)
  (with-bknr-page (:title #?"edit-images")
    (search-image-collection :title "search images")
    (edit-image-collection :title "edit images")))

(defmethod handle-object-form ((handler edit-images-handler)
			       (action (eql :delete)) images)
  (let ((names (mapcar #'store-image-name images)))
    (mapc #'delete-object images)
    (setf (session-value :current-query-result)
	  (set-difference (session-value :current-query-result)
			  images))
    (with-bknr-page (:title #?"delete images")
      (html (:h2 "Deleted images:")
	  (dolist (name names)
	    (html (:princ-safe name)(:br)))))))

(defmethod handle-object-form ((handler edit-images-handler) (action (eql :add-keywords))
			       images)
  (let ((keywords (keywords-from-query-param-list (query-param-list "keyword"))))
    (dolist (image images)
      (store-object-add-keywords image 'keywords keywords))
    (with-bknr-page (:title #?"edit-all-images")	
      (image-collection
       images
       :title (format nil "Added keywords ~a to images:" keywords)))))

(defmethod handle-object-form ((handler edit-images-handler) (action (eql :remove-keywords))
			       images)
  (let ((keywords (keywords-from-query-param-list (query-param-list "keyword"))))
    (dolist (image images)
      (store-object-remove-keywords image 'keywords keywords))
    (with-bknr-page (:title #?"edit-all-images")	
      (image-collection
       images
       :title (format nil "Removed keywords ~a from images:" keywords)))))

(defmethod handle-object-form ((handler edit-images-handler)
			       (action (eql :assign-individual-keywords))
			       images)
  (let* ((keywords (keywords-from-query-param-list (query-param-list "keyword-img")
						   :remove-empty nil))
	 (assigned-images (loop for keys on keywords by #'cddr
				for keyword = (if (eq (car keys) :||)
						  (cadr keys)
						  (car keys))
				for image in images
				when (and (not (eq keyword :||))
					  image)
				do (store-object-add-keywords image 'keywords (list keyword))
				and collect image)))
    (with-bknr-page (:title #?"edit-all-images")
      (image-collection assigned-images
				       :title "Add keywords to images:"))))

(defmethod handle-object-form ((handler edit-images-handler)
			       (action (eql :search))
			       images)
  (let* ((keywords (remove :|| (keywords-from-query-param-list (query-param-list "keyword"))))
	 (operator (query-param "operator"))
	 (name (query-param "name")))
    (with-bknr-page (:title #?"edit-images")
      (search-image-collection :title "search images")
      (setf (session-value :current-query-result)
	    (if keywords
		(ecase (make-keyword-from-string operator)
		  (:or (get-keywords-union-store-images keywords))
		  (:and (get-keywords-intersection-store-images keywords)))
		(all-store-images)))
      (when name
	(setf (session-value :current-query-result)
	      (remove-if-not #'(lambda (img)
				 (scan name (store-image-name img)))
			     (session-value :current-query-result))))
      (edit-image-collection))))

(defclass edit-image-handler (edit-object-handler image-handler)
  ((require-user-flag :initform :admin)))

(defun show-image-editor (image)
  (let ((image-id (store-object-id image)))
    (html ((:form :method "POST")
	   (image-browser :id image-id)
	   (html (:div
		  (submit-button "delete" "Delete image"
				 :confirm "Really delete image?"))
		 (:div
		  "Assign keyword: " (image-keyword-choose-dialog :size "1" :create t)
		  "Remove keywords: " (dolist (keyword (mapcar #'string-downcase (mapcar #'symbol-name (store-image-keywords image))))
					(html ((:input :type "checkbox" :name "remove-keyword" :value #?"${keyword}"))
					      (:princ-safe keyword))))
		 (:div
		  (submit-button "edit" "Edit keywords")))))))

(defmethod handle-object-form ((handler edit-image-handler) action image)
  (let ((image-name (store-image-name image)))
    (with-bknr-page (:title #?"bknr image ${image-name}")
      (show-image-editor image))))

(defmethod handle-object-form ((handler edit-image-handler)
			       (action (eql :delete)) image)
  (let ((name (store-image-name image)))
    (delete-object image)
    (with-bknr-page (:title #?"delete images")
      (html (:h2 "Deleted image " (:princ-safe name))))))

(defmethod handle-object-form ((handler edit-image-handler)
			       (action (eql :edit)) image)
  (let ((name (store-image-name image))
	(add-keywords (keywords-from-query-param-list (query-param-list "keyword")))
	(remove-keywords (keywords-from-query-param-list (query-param-list "remove-keyword"))))
  (with-bknr-page (:title #?"edit image $(name)")
    (when remove-keywords
      (store-object-remove-keywords image 'keywords remove-keywords)
      (html (:h2 (:princ (format nil "Removed keywords ~a from image" remove-keywords)))))
    (when add-keywords
      (store-object-add-keywords image 'keywords add-keywords)
      (html (:h2 (:princ (format nil "Added keywords ~a to image" add-keywords)))))
    (unless (or add-keywords remove-keywords)
      (html (:h2 "No keywords added or removed")))
    (show-image-editor image))))