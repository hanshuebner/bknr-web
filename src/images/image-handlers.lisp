(in-package :bknr.images)

(enable-interpol-syntax)

(defun emit-image-to-browser (image image-format &key (quality -1) (date 0) cache-sticky max-age)
  (with-http-response (:content-type (image-content-type image-format))
    (when cache-sticky
      (setf (header-out :expires) (rfc-1123-date (+ (get-universal-time) (* 365 24 60 60)))))
    (when max-age
      (setf (header-out :cache-control) (format nil "max-age=~A" max-age)))
    (unless (zerop date)
      (setf (header-out :last-modified) (rfc-1123-date date)))
    (let ((stream (send-headers)))      
      (setf (save-alpha-p :image image) t)
      (if (member image-format '(:jpg :jpeg))
	  (write-image-to-stream stream image-format :image image :quality quality)
	  (write-image-to-stream stream image-format :image image))
      (finish-output stream))))

(defmethod store-image-xml-info ((image store-image))
  (cxml:with-element "image"
    (cxml:attribute "id" (store-object-id image))
    (cxml:attribute "timestamp" (format-date-time (blob-timestamp image) :xml-style t))
    (cxml:attribute "name" (store-image-name image))
    (cxml:attribute "format" (blob-type image))
    (cxml:attribute "width" (store-image-width image))
    (cxml:attribute "height" (store-image-height image))
    (dolist (keyword (store-image-keywords image))
      (cxml:with-element "keyword"
	(cxml:text (string-downcase (symbol-name keyword)))))))

(defclass image-handler (object-handler)
  ())

(defmethod object-handler-get-object ((handler image-handler))
  (let ((id-or-name (parse-url)))
    (when id-or-name
      (find-store-object id-or-name :class 'store-image :query-function #'store-image-with-name))))

(defclass browse-image-handler (image-handler)
  ())

(defmethod handle-object ((page-handler browse-image-handler) image)
  (let ((image-name (store-image-name image))
	(image-id   (store-object-id image)))
    (with-bknr-page (:title #?"bknr image ${image-name}")
      (image-browser :id image-id))))

(defclass image-page-handler (object-list-handler)
  ())

(defmethod object-list-handler-title ((handler image-page-handler) object)
  "bknr images")

(defmethod object-list-handler-get-objects ((handler image-page-handler) object)
  (all-store-images))

(defun make-keyword-results (images)
  (loop for i on images by (curry #'nthcdr 30)
        collect (subseq* i 0 30)))

(defmethod handle-object ((handler image-page-handler) images)
  (let ((results (make-keyword-results (object-list-handler-get-objects handler images))))
    (with-bknr-page (:title (object-list-handler-title handler images))
      (image-page results))))
  
(defclass upload-image-handler (form-handler)
  ())

(defmethod handle-form ((handler upload-image-handler)
			(action null))
  (with-bknr-page (:title "Image upload")
    (html "Please upload your image"
	  ((:form :enctype "multipart/form-data" :method "post")
	   "File: " ((:input :type "file" :name "file")) :br
	   ((:input :type "submit" :name "action" :value "upload"))))))

(defmethod handle-form ((handler upload-image-handler)
			(action (eql :upload)))
  (with-bknr-page (:title "Image upload result")
    (let ((upload (request-uploaded-file "file")))
      (unless upload
	(error "no file uploaded"))
      (with-query-params (name keyword)
	(let* ((image (bknr.images:import-image (pathname upload)
                                                :user (bknr-session-user)
                                                :keywords (when keyword (list keyword))
                                                :keywords-from-dir nil))
	       (image-id (store-object-id image)))
	  (if image
	      (html "Image successfully imported"
		    :br
		    (cmslink (format nil #?"~a/${image-id}"
				     "/edit-image")
		     ((:img :src (image-url image :process "thumbnail")
			    :alt name))))
	      (html "Could not import image")))))))

(defclass image-keyword-handler (image-page-handler keyword-handler)
  ())

(defmethod handle-object ((handler image-keyword-handler)
			  (keyword (eql nil)))
  (with-bknr-page (:title "No keyword was given")
    (html "No keyword was given!")))

(defmethod object-list-handler-get-objects ((handler image-keyword-handler) keyword)
  (get-keyword-store-images keyword))

(defmethod object-list-handler-title ((handler image-keyword-handler) keyword)
  (format nil "bknr keyword images: ~a" keyword))

(defclass image-union-handler (image-page-handler keywords-handler)
  ())

(defmethod object-list-handler-get-objects ((handler image-union-handler) keywords)
  (get-keywords-union-store-images keywords))

(defmethod object-list-handler-title ((handler image-union-handler) keywords)
  (format nil "bknr union images: ~a" keywords))

(defclass image-intersection-handler (image-page-handler keywords-handler)
  ())

(defmethod object-list-handler-get-objects ((handler image-intersection-handler) keywords)
  (get-keywords-intersection-store-images keywords))

(defmethod object-list-handler-title ((handler image-intersection-handler) keywords)
  (format nil "bknr intersection images: ~a" keywords))


(defclass xml-image-browser-handler (image-handler xml-object-handler)
  ())

(defmethod xml-object-handler-show-object ((handler xml-image-browser-handler) image)
  (store-image-xml-info image))

(defclass xml-image-query-handler (xml-object-list-handler)
  ())

(defmethod object-list-handler-get-objects ((handler xml-image-query-handler) keywords)
  (if keywords
      (get-keywords-intersection-store-images (mapcar #'make-keyword-from-string keywords))
      (class-instances 'store-image)))

(defmethod object-list-handler-show-object-xml ((handler xml-image-query-handler) image)
  (store-image-xml-info image))

(define-bknr-webserver-module images
    ("/edit-images" edit-images-handler)
  ("/edit-image" edit-image-handler)
  ("/browse-image" browse-image-handler)
  ("/image-page" image-page-handler)
  ("/upload-image" upload-image-handler)
  ("/image-keyword" image-keyword-handler)
  ("/image-union" image-union-handler)
  ("/image-intersection" image-intersection-handler)
  ("/image" imageproc-handler)
  ("/image-import" image-import-handler)
  ("/session-image" session-image-handler)
  ("/xml-image-query" xml-image-query-handler)
  ("/xml-image-browser" xml-image-browser-handler))
