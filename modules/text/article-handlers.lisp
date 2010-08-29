(in-package :bknr.text)

;;; links
(defmethod edit-object-url ((article article))
  (format nil "/edit-article/~A" (store-object-id article)))

(defmethod object-url ((article article))
  (format nil "/article/~A" (store-object-id article)))

(defmethod html-link ((article article))
  (html
   ((:a :href (object-url article))
    (:princ (format nil "&nbsp;~a&nbsp;" (article-subject article))))))

(defmethod html-edit-link ((article article))
  (html
   (cmslink (edit-object-url article)
            (:princ (format nil "edit: \"~a\"" (article-subject article))))))


;;; handlers
(defun article-page ()
  (with-bknr-page (:title "article")
    (article :id (parse-url))))

(defclass edit-article-handler (edit-object-handler)
  ()
  (:default-initargs :object-class 'article))

(defmethod handle-object-form ((handler edit-article-handler)
			       action article)
  (with-bknr-page (:title "edit article")
    (article-form :id (when article (store-object-id article)))))

(defmethod handle-object-form ((handler edit-article-handler)
			       (action (eql :save))
                               (article article))
  (with-query-params (subject text)
    (with-transaction (:update-article)
      (setf (article-text article) text
            (article-subject article) subject))
    (index-article article))
  (redirect (edit-object-url article)))

(defmethod handle-object-form ((handler edit-article-handler)
			       (action (eql :save))
                               (article (eql nil)))
  (with-query-params (subject text)
    (redirect (edit-object-url (make-instance 'article
                                              :author (bknr-session-user)
                                              :subject subject
                                              :text text)))))

;;; snippets
(defmethod edit-object-url ((snippet snippet))
  (format nil "/edit-snippet/~A" (store-object-id snippet)))

(defclass edit-snippet-handler (edit-object-handler)
  ()
  (:default-initargs :object-class 'snippet))

(defmethod handle-object-form ((handler edit-snippet-handler)
			       action snippet)
  (with-bknr-page (:title "edit snippet")
    (snippet-form :id (when snippet (store-object-id snippet)))
    (unless snippet
      (html (:h2 "snippets: ")
	    (:ul (dolist (snippet (sort (all-snippets) #'> :key #'article-time))
		   (html (:li (html-edit-link snippet)))))))))

(defmethod handle-object-form ((handler edit-snippet-handler)
			       action (snippet (eql nil)))
  (redirect "/edit-snippet"))

(defmethod handle-object-form ((handler edit-snippet-handler)
			       (action (eql :delete)) (snippet snippet))
  (delete-object snippet)
  (call-next-method))

(defmethod handle-object-form ((handler edit-snippet-handler)
			       (action (eql :remove-keywords)) (snippet snippet))
  (let ((keywords (keywords-from-query-param-list (query-param-list "keyword"))))
    (store-object-remove-keywords snippet 'keywords keywords)
    (redirect (edit-object-url snippet))))

(defmethod handle-object-form ((handler edit-snippet-handler)
			       (action (eql :add-keywords)) (snippet snippet))
  (let ((keywords (keywords-from-query-param-list (query-param-list "keyword"))))
    (store-object-add-keywords snippet 'keywords keywords)
    (redirect (edit-object-url snippet))))

(defmethod handle-object-form ((handler edit-snippet-handler)
			       (action (eql :save)) (snippet snippet))
  (with-query-params (subject text layout)
    (unless subject (setf subject ""))
    (let ((expires (parse-date-field "expiration")))
      (with-transaction (:update-snippet)
        (setf (article-subject snippet) subject
              (article-text snippet) text
              (snippet-expires snippet) expires
              (snippet-layout snippet) (make-keyword-from-string layout)))
      (index-article snippet)
      (redirect (edit-object-url snippet)))))

(defmethod handle-object-form ((handler edit-snippet-handler)
			       (action (eql :create)) snippet)
  (let ((keywords (keywords-from-query-param-list (query-param-list "keyword")))
	(expires (parse-date-field "expiration")))
    (with-query-params (subject text layout)
      (let ((snippet (make-instance 'snippet :author (bknr-session-user)
                                             :subject (or subject "")
                                             :time (get-universal-time)
                                             :text text
                                             :keywords keywords
                                             :layout (make-keyword-from-string layout)
                                             :expires expires)))
	(redirect (edit-object-url snippet))))))
