(in-package :bknr-user)

(enable-interpol-syntax)

(defclass edit-feed-handler (edit-object-handler feed-handler)
  ((require-user-flag :initform :feed)))

(defmethod handle-object-form ((handler edit-feed-handler) action (feed (eql nil)))
  (with-bknr-page (:title "create feed")
    (:h2 "Manage feeds")
    (:ul (loop for feed in (all-feeds)
            do (html (:li ((:a :href (format nil "/edit-feed/~a"
                                             (feed-name feed)))
                           (:princ-safe (feed-name feed)))))))
    (:h2 "Create feed")
    (feed-form)))

(defmethod handle-object-form ((handler edit-feed-handler) action feed)
  (let ((feed-name (feed-name feed)))
    (with-bknr-page (:title #?"edit feed: ${feed-name}")
      (:h2 #?"Edit feed: ${feed-name}")
      (feed-form :feed-id (store-object-id feed)))))

(defmethod handle-object-form ((handler edit-feed-handler) (action (eql :create)) obj)
  (with-query-params (name url refresh type encoding)
    (if (and name url type)
	(let* ((keywords (keywords-from-query-param-list (query-param-list "keyword")))
	       (feed (make-instance 'feed
                                    :name name
                                    :url url
                                    :refresh (if refresh
                                                 (parse-integer refresh)
                                                 3600)
                                    :type (make-keyword-from-string type)
                                    :encoding (make-keyword-from-string encoding)
                                    :keywords keywords)))
	  (redirect (format nil "/edit-feed/~a" (feed-name feed))))
	(handle-object-form handler nil nil))))

(defmethod handle-object-form ((handler edit-feed-handler)
			       (action (eql :add-keywords))
			       feed)
  (when feed
    (let ((keywords (keywords-from-query-param-list
		     (query-param-list "keyword"))))
      (store-object-add-keywords feed 'keywords keywords)))
  (call-next-method))

(defmethod handle-object-form ((handler edit-feed-handler)
			       (action (eql :remove-keywords))
			       feed)
  (when feed
    (let ((keywords (keywords-from-query-param-list
		     (query-param-list "keyword"))))
      (store-object-remove-keywords feed 'keywords keywords)))
  (call-next-method))

(defmethod handle-object-form ((handler edit-feed-handler)
			       (action (eql :update))
			       feed)
  (when feed (update-feed feed :force t))
  (call-next-method))

(defmethod handle-object-form ((handler edit-feed-handler)
			       (action (eql :save))
			       feed)
  (when feed 
    (with-query-params (url refresh type encoding)
      (when (and url (not (string-equal url (feed-url feed))))
	(feed-change-url feed url))
      (when refresh (change-slot-values feed 'refresh-interval (parse-integer refresh)))
      (when type (change-slot-values feed 'type (make-keyword-from-string type)))
      (when encoding (change-slot-values feed 'encoding (make-keyword-from-string encoding)))))
  (call-next-method))

(defmethod handle-object-form ((handler edit-feed-handler)
			       (action (eql :delete))
			       feed)
  (when feed (delete-object feed))
  (redirect "/edit-feed"))
