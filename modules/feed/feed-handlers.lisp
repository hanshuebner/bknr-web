(in-package :bknr-user)

(enable-interpol-syntax)

(defun feed-list (feeds)
  (html (:ul (loop for feed in feeds
		   do (html (:li ((:a :href (format nil "/feed/~a"
						    (feed-name feed)))
				  (:princ-safe (feed-name feed)))
				 ((:a :href (format nil "/feed-rss/~a"
						    (feed-name feed)))
				  " (rss)")))))))

(defun rss-feed-group-items (feed)
  (sort (group-on (rss-feed-items feed)
		  :key #'(lambda (item) (get-daytime (rss-item-date item))))
	#'> :key #'car))

(defclass feed-handler (object-handler)
  ())

(defmethod object-handler-get-object ((handler feed-handler))
  (let ((id-or-name (parse-url)))
    (find-store-object id-or-name :class 'feed)))

(defmethod handle-object ((handler feed-handler) (feed (eql nil)))
  (with-bknr-page (:title #?"bknr feed aggregator: all feeds")
    (feed-list (all-feeds))))

(defmethod handle-object ((handler feed-handler) feed)
  (let ((feed-name (feed-name feed))
	(rss-feed (feed-rss-feed feed)))
    (with-bknr-page (:title #?"bknr feed aggregator: ${feed-name}")
      (feed :feed-id (store-object-id feed))
      (when rss-feed
	(rss-feed-page (rss-channel-link (rss-feed-channel rss-feed))
				     (rss-channel-title (rss-feed-channel rss-feed))
				     (rss-feed-group-items rss-feed))))))


(defclass feed-list-handler (object-date-list-handler)
  ())

(defmethod object-date-list-handler-grouped-objects ((handler feed-list-handler)
						     object)
  (let* ((title (object-list-handler-title handler object))
	 (feeds (object-list-handler-get-objects handler object))
	 (rss-feed (merge-feeds title (render-uri (script-name*) nil)
				title (remove nil (mapcar #'feed-rss-feed feeds))))
	 (grouped-items (rss-feed-group-items rss-feed)))
    grouped-items))

(defmethod handle-object ((handler feed-list-handler) foo)
  (let ((title (object-list-handler-title handler foo))
	(rss-link (object-list-handler-rss-link handler foo))
	(grouped-items (object-date-list-handler-grouped-objects handler foo)))
    (with-bknr-page (:title title)
      (html ((:a :href rss-link) "rss")
	    (rss-feed-page rss-link title grouped-items)))))

(defclass feed-keyword-handler (feed-list-handler keyword-handler)
  ())

(defmethod object-list-handler-title ((handler feed-keyword-handler) keyword)
  (format nil "feeds with keyword: ~a" keyword))

(defmethod object-list-handler-rss-link ((handler feed-keyword-handler) keyword)
  (format nil "/feed-keyword-rss/~a" keyword))

(defmethod object-list-handler-get-objects ((handler feed-keyword-handler) keyword)
  (get-keyword-feeds keyword))

#+xxx
(defmethod handle-object ((handler feed-keyword-handler) (keyword (eql nil)))
  (with-bknr-page (:title "all-feed-keywords")
    (:ul (loop for keyword in (index-keys (current-store) :feed-keywords-key)
	       for name = (string-downcase (symbol-name keyword))
	       do (html (:li
			 ((:a :href (format nil "/feed-keyword/~a" name))
			  (:princ-safe name))
			 ((:a :href (format nil "/feed-keyword-rss/~a" name))
			  "(rss)")))))))

(defclass feed-union-handler (feed-list-handler keywords-handler)
  ())

(defmethod object-list-handler-title ((handler feed-union-handler) keywords)
  (format nil "feeds with keywords: ~a" keywords))

(defmethod object-list-handler-rss-link ((handler feed-union-handler) keywords)
  (format nil "/feed-union-rss/~A"
	  (parse-url)))

(defmethod object-list-handler-get-objects ((handler feed-union-handler) keywords)
  (get-keywords-union-feeds keywords))


(defclass feed-intersection-handler (feed-list-handler keywords-handler)
  ())

(defmethod object-list-handler-title ((handler feed-intersection-handler) keywords)
  (format nil "feeds with all keywords: ~a" keywords))

(defmethod object-list-handler-rss-link ((handler feed-intersection-handler) keywords)
  (format nil "/feed-intersection-rss/~A"
	  (parse-url)))

(defmethod object-list-handler-get-objects ((handler feed-intersection-handler) keywords)
  (get-keywords-intersection-feeds keywords))


;;; rss handlers

(defclass rss-feed-handler (object-rss-handler feed-handler)
  ())

(defmethod create-object-rss-feed ((handler rss-feed-handler) (feed (eql nil)))
  (make-instance 'rss-feed :channel (make-instance 'rss-channel
						   :about "no such feed"
						   :title "no such feed")))

(defmethod create-object-rss-feed ((handler rss-feed-handler) feed)
  (if (feed-rss-feed feed)
      (feed-rss-feed feed)
      (make-instance 'rss-feed
		     :channel (make-instance
			       'rss-channel
			       :about (format nil "could not update ~a"
					      (feed-name feed))
			       :title (feed-name feed)))))

(defclass rss-feed-list-handler (object-rss-handler feed-list-handler)
  ())

(defmethod create-object-rss-feed ((handler rss-feed-list-handler) keyword)
  (let ((feeds (object-list-handler-get-objects handler keyword)))
    (merge-feeds (object-list-handler-title handler keyword)
		 (render-uri (script-name*) nil)
		 (object-list-handler-title handler keyword)
		 (remove nil (mapcar #'feed-rss-feed feeds)))))

(defclass rss-feed-keyword-handler (rss-feed-list-handler feed-keyword-handler)
  ())

(defclass rss-feed-union-handler (rss-feed-list-handler feed-union-handler)
  ())

(defclass rss-feed-intersection-handler (rss-feed-list-handler feed-intersection-handler)
  ())

