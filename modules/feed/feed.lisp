(in-package :bknr-user)

(define-persistent-class feed ()
  ((url :update
	:index-type string-unique-index
	:index-reader feed-with-url
	:index-values all-feeds)
   (name :read
	 :index-type string-unique-index
	 :index-reader feed-with-name)
   (keywords :update :initform nil
                     :index-type hash-list-index
                     :index-reader get-keyword-feeds
                     :index-keys all-feed-keywords)
   (subscribed-users :update :initform nil
                             :index-type hash-list-index
                             :index-reader get-user-feeds)
		     
   
   (last-updated :update :transient t :initform 0)
   (rss-feed :update :transient t :initform nil)
   (refresh-interval :update :initform 3600)
   (article-dates :update :initform (make-hash-table :test #'equal))
   (type :update :documentation "(or :rss091 :rss10 :rss20 :atom)")
   (encoding :update :initform :iso-8859-1 :documentation "(or :utf8 :iso-8859-1)")))

(defmethod print-object ((object feed) stream)
  (format stream "#<~a ID: ~A \"~a\">"
	  (class-name (class-of object))
	  (store-object-id object)
	  (feed-name object))
  object)

(defun get-keywords-union-feeds (keywords)
  (reduce #'union (mapcar #'get-keyword-feeds keywords)))

(defun get-keywords-intersection-feeds (keywords)
  (reduce #'intersection (mapcar #'get-keyword-feeds keywords)))

(deftransaction feed-article-date (feed article date)
  (setf (gethash article (feed-article-dates feed))
	date))

(defun feed-set-article-dates (feed)
  (let ((rss-feed (feed-rss-feed feed)))
    (when rss-feed
      (dolist (item (rss-feed-items rss-feed))
	(when (= (rss-item-date item) 0)
	  (let ((date (gethash (rss-item-link item) (feed-article-dates feed))))
	    (if date
		(setf (rss-item-date item) date)
		(setf (rss-item-date item)
		      (feed-article-date feed (rss-item-link item)
					 (get-universal-time))))))))))

#+(or)
(defmethod update-feed ((feed feed) &key (force nil))
  (let ((time (get-universal-time)))
    (if (or (> (- time (feed-last-updated feed))
	       (feed-refresh-interval feed))
	    force)
	(multiple-value-bind (xml-feed code)
	    (net.aserve.client:do-http-request (feed-url feed) :protocol :http/1.0)
	  (if (= code 200)
	      (progn (setf xml-feed (delete #\Return xml-feed))
		     (when (eq (feed-encoding feed) :utf-8)
		       (setf xml-feed (convert-utf8-to-latin1 xml-feed)))
		     (setf (feed-rss-feed feed)
			   (let ((*base-url* (parse-uri (feed-url feed))))
			     (case (feed-type feed)
			       (:rss091 (parse-rss091-feed xml-feed))
			       (:rss10 (parse-rss10-feed xml-feed))
			       (:atom (parse-atom-feed xml-feed))
			       (:rss20 (parse-rss20-feed xml-feed))))))
	      (setf (feed-rss-feed feed) nil))
	  (setf (feed-last-updated feed) time)
	  (feed-set-article-dates feed)))))

(defun update-all-feeds (&key force)
  (loop for feed in (all-feeds)
     do (ignore-errors (update-feed feed :force force))))

(deftransaction feed-change-url (feed url)
  (setf (feed-url feed) url))
					     
(deftransaction feed-subscribe-user (feed user)
  (setf (feed-subscribed-users feed) (cons user (feed-subscribed-users feed))))

(deftransaction feed-unsubscribe-user (feed user)
  (setf (feed-subscribed-users feed) (remove user (feed-subscribed-users feed))))

#+(or)
(unless (cron-job-with-name "update weblogs")
  (make-instance 'cron-job :name "update weblogs"
                 :minute 0))
