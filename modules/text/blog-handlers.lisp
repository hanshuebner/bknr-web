(in-package :bknr.text)

(enable-interpol-syntax)

;;; links
(defmethod object-url ((blog blog))
  (format nil "/blog/~A" (blog-name blog)))

(defmethod edit-object-url ((blog blog))
  (format nil "/edit-blog/~A" (blog-name blog)))

(defmethod html-link ((blog blog))
  (cmslink (object-url blog)
           (:princ (blog-name blog))))

(defmethod html-edit-link ((blog blog))
  (cmslink (edit-object-url blog)
           (:princ (format nil "edit ~a" (blog-name blog)))))

;;; handlers
(defclass blog-handler (object-date-list-handler)
  ())

(defmethod object-handler-get-object ((handler blog-handler))
  (let ((id-or-name (parse-url)))
    (when id-or-name
      (find-store-object id-or-name :class 'blog :query-function #'blog-with-name))))

(defmethod object-list-handler-get-objects ((handler blog-handler) blog)
  (let ((date (next-day 1 :start (object-date-list-handler-date handler blog))))
    (remove-if #'(lambda (article)
		   (> (article-time article) date))
	       (blog-articles blog))))

(defmethod object-date-list-handler-grouped-objects ((handler blog-handler) blog)
  (sort (group-on (object-list-handler-get-objects handler blog)
		  :key #'(lambda (article) (get-daytime (article-time article))))
	#'> :key #'car))

(defmethod handle-object ((handler blog-handler) (blog (eql nil)))
  (with-bknr-page (:title "blogs")
    (:ul (loop for blog in (sort (all-blogs) #'string< :key #'blog-name)
            do (html (:li (cmslink (format nil "/blog/~a" (blog-name blog))
                                   (:princ-safe (blog-name blog)))))))))

(defmethod handle-object ((handler blog-handler) blog)
  (let ((grouped-articles (object-date-list-handler-grouped-objects handler blog))
	(name (blog-name blog)))
    (with-bknr-page (:title #?"blog: ${name}")
      (blog-page blog grouped-articles
                 :start-date (object-date-list-handler-date handler blog)))))

(defclass search-blog-handler (edit-object-handler blog-handler)
  ())

(defmethod authorized-p ((handler search-blog-handler))
  t)

(defmethod handle-object-form ((handler search-blog-handler) action
			       (blog (eql nil)))
  (with-bknr-page (:title "search blogs")
    (:ul (loop for blog in (sort (all-blogs) #'string< :key #'blog-name)
            do (html (:li (cmslink (format nil "/search-blog/~a"
                                           (blog-name blog))
                                   (:princ-safe (blog-name blog)))))))))

(defmethod handle-object-form ((handler search-blog-handler) action
			       blog)
  (with-bknr-page (:title #?"search blog $((blog-name blog))")
    (cmslink (format nil "/blog/~A" (blog-name blog))
             "return to " (:princ-safe (blog-name blog)))
    (blog-search-form :title #?"search blog $((blog-name blog))")
    (blog-search-results)))

(defmethod handle-object-form ((handler search-blog-handler) (action (eql :search))
			       blog)
  (with-query-params (search)
    (when (and blog search)
      (setf (session-value :blog-search-results)
	    (search-blog blog search :threshold 0.01)
	    (session-value :blog-search) search)))
  (handle-object-form handler nil blog))

(defclass edit-blog-handler (edit-object-handler blog-handler)
  ())

(defmethod authorized-p ((handler edit-blog-handler))
  (let ((user (bknr-session-user))
	(blog (object-handler-get-object handler)))
    (if blog
	(or (admin-p user)
	    (member user (blog-owners blog)))
	t)))

(defmethod handle-object-form ((handler edit-blog-handler) action (blog (eql nil)))
  (with-bknr-page (:title "edit blogs")
    (:ul (loop for blog in (sort (all-blogs) #'string< :key #'blog-name)
            do (html (:li (html-edit-link blog)))))))

(defmethod handle-object-form ((handler edit-blog-handler) action blog)
  (with-bknr-page (:title "new blog article")
    (:h2 "New article")
    (article-form)
    (:h2 "Old articles")
    (:ul (loop for article in (sort (copy-list (blog-articles blog)) #'> :key #'article-time)
            do (html (:li (html-edit-link article)))))))

(defmethod handle-object-form ((handler edit-blog-handler) (action (eql :save)) blog)
  (with-query-params (article-id subject text keyword)
    (if article-id
	(let ((article (find-store-object article-id :class 'blog-article)))
	  (when article
	    (change-slot-values (store-object-with-id (parse-integer article-id))
				'subject subject 'text text)
	    (index-article article)))
	(let ((article (make-instance 'blog-article
                                      :time (get-universal-time)
                                      :author (bknr-session-user)
                                      :subject subject
                                      :text text
                                      :keywords (list keyword))))
	  (blog-add-article blog article)))
    (handle-form handler t)))

(define-bknr-webserver-module blog
    ("/blog" blog-handler)
  ("/edit-blog" edit-blog-handler)
  ("/edit-article" edit-article-handler)
  ("/search-blog" search-blog-handler))
