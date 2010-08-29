(in-package :bknr-user)

(enable-interpol-syntax)

(define-bknr-tag album (&key username album)
  (let* ((user (find-user username))
	 (images (when user
		   (remove-if-not #'(lambda (image)
				      (eq user (owned-object-owner image)))
				  (get-keyword-store-images
				   (make-keyword-from-string album))))))
    (html (:ul (dolist (image images)
		 (html (:li ((:img :src (format nil "/image/~A/thumbnail,,640,480"
						(store-object-id image)))))))))))

(define-bknr-tag user-albums (&key username)
  (let* ((user (find-user username))
	 (albums (when user
		   (reduce #'union
			   (remove-if-not #'(lambda (o) (typep o 'store-image))
					  (store-objects-owned-by user))
			   :key #'store-image-keywords))))
    (html (:ul (dolist (album albums)
		 (let ((name (string-downcase (symbol-name album))))
		   (html (:li ((:a :href (format nil "/album/~A/~A"
						 username name))
			       (:princ-safe name))))))))))
    

(in-package :bknr-user)

(defclass album-handler (prefix-handler)
  ())

(defmethod handle ((handler album-handler))
  (multiple-value-bind (username album)
      (parse-handler-url handler)
    (let ((user (when username (find-user username))))
      (cond ((and user album)
	     (with-bknr-page (:title #?"${username} : ${album}")
	       (album :username username :album album)))
	    (user
	     (with-bknr-page (:title #?"${username}'s albums")
	       (user-albums :username username)))
	    (t (with-bknr-page (:title "No such album")
		 (:h2 "No such album")))))))

