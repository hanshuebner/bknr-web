(in-package :bknr-user)

(enable-interpol-syntax)

(defmethod object-url ((url url))
  (format nil "/url/~A"
	  (store-object-id url)))

(defclass url-handler (object-handler)
  ())

(defmethod object-handler-get-object ((handler url-handler))
  (find-store-object (parse-url) :class 'url))

(defmethod handle-object ((handler url-handler) (url (eql nil)))
  (redirect "/url-page"))

(defmethod handle-object ((handler url-handler) url)
  (let ((submissions (sort (group-on (url-submissions url)
				     :key #'(lambda (submission)
					      (get-daytime (url-submission-date submission))))
			   #'> :key #'car)))
    (with-bknr-page (:title #?"url page for $((url-url url))")
      (:p "keywords: "
	  (mapc #'url-keyword-link (url-keywords url)))
      (:p (url-submissions-page submissions :full t)))))

(defclass url-redirect-handler (url-handler)
  ())

(defmethod handle-object ((handler url-redirect-handler) url)
  (if url
      (redirect (url-url url))
      (redirect "/url-page")))

(defclass url-page-handler (object-date-list-handler)
  ())

(defmethod object-list-handler-title ((handler url-page-handler)
				      object)
  "bknr urls")

(defmethod object-list-handler-rss-link ((handler url-page-handler)
					 object)
  "/url-rss")

(defmethod object-list-handler-get-objects ((handler url-page-handler) object)
  (mapcar #'url-latest-submission (all-urls)))

(defmethod object-date-list-handler-grouped-objects ((handler url-page-handler)
						     object)
  (let* ((date (next-day 1 :start (object-date-list-handler-date handler object)))
	 (submissions (remove-if #'(lambda (submission)
				     (> (url-submission-date submission) date))
				 (object-list-handler-get-objects handler object))))
    (sort (group-on submissions
		    :key #'(lambda (submission)
			     (get-daytime (url-submission-date submission))))
	  #'> :key #'car)))

(defmethod handle-object ((handler url-page-handler) object)
  (let ((submissions (object-date-list-handler-grouped-objects handler object)))
    (with-bknr-page (:title (object-list-handler-title
				     handler object))
      (:p "random keywords: " (url-random-keywords))
      (:p ((:a :href (object-list-handler-rss-link handler object)) "rss")
	  " "
	  ((:a :href "/submit-url") "submit an url"))
      (url-submissions-page
       submissions
       :start-date  (object-date-list-handler-date handler object)))))

(defclass url-submitter-handler (url-page-handler)
  ())

(defmethod object-handler-get-object ((handler url-submitter-handler))
  (find-store-object (parse-url) :class 'user
		     :query-function #'find-user))

(defmethod object-list-handler-get-objects ((handler url-submitter-handler)
					     user)
  (copy-list (get-user-url-submissions user)))

(defmethod object-list-handler-title ((handler url-submitter-handler)
				      user)
  (format nil "bknr urls submitted by ~a" (user-full-name user)))

(defmethod object-list-handler-rss-link ((handler url-submitter-handler)
					 user)
  (format nil "/url-submitter-rss/~A"
	  (user-login user)))

(defmethod handle-object ((handler url-submitter-handler) user)
  (let ((submissions (object-date-list-handler-grouped-objects handler user)))
    (with-bknr-page (:title (object-list-handler-title
				     handler user))
      ((:a :href (object-list-handler-rss-link handler user)) "rss")
      (url-submissions-page submissions
					  :start-date  (object-date-list-handler-date
							handler user)
					  :url (format nil "/url-submitter/~A"
						       (user-login user))))))

(defclass url-keyword-handler (url-page-handler keyword-handler)
  ())

(defmethod object-list-handler-get-objects ((handler url-keyword-handler)
					     keyword)
  (mapcar #'url-latest-submission (get-keyword-urls keyword)))

(defmethod handle-object ((handler url-keyword-handler)
			  (keyword (eql nil)))
  (with-bknr-page (:title "all-url-keywords")
    (:ul (dolist (keyword (all-url-keywords))
	   (html (:li (url-keyword-link keyword)))))))

(defmethod object-list-handler-title ((handler url-keyword-handler)
				      keyword)
  (format nil "bknr keyword urls: ~a" keyword))

(defmethod object-list-handler-rss-link ((handler url-keyword-handler)
					 keyword)
  (format nil "/url-keyword-rss/~A"
	  (string-downcase (symbol-name keyword))))


(defclass url-union-handler (url-page-handler keywords-handler)
  ())

(defmethod object-list-handler-get-objects ((handler url-union-handler)
					keywords)
  (mapcar #'url-latest-submission (get-keywords-union-urls keywords)))

(defmethod object-list-handler-title ((handler url-union-handler)
				      keywords)
  (format nil "bknr union keyword urls: ~a" keywords))

(defmethod object-list-handler-rss-link ((handler url-union-handler)
					 keyword)
  (format nil "/url-union-rss/~A"
	  (parse-url)))

(defclass url-intersection-handler (url-page-handler keywords-handler)
  ())

(defmethod object-list-handler-get-objects ((handler url-intersection-handler)
					keywords)
  (mapcar #'url-latest-submission (get-keywords-intersection-urls keywords)))

(defmethod object-list-handler-title ((handler url-intersection-handler)
				      keywords)
  (format nil "bknr intersection keyword urls: ~a" keywords))

(defmethod object-list-handler-rss-link ((handler url-intersection-handler)
					 keyword)
  (format nil "/url-intersection-rss/~A"
	  (parse-url)))

