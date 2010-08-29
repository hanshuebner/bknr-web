(in-package :bknr-user)

(enable-interpol-syntax)

(define-bknr-tag feed-keyword-choose-dialog
    (&key (size "4") (name "keyword") (create nil))
  (let ((size (or (parse-integer size :junk-allowed t) 1)))
    (keyword-choose-dialog (all-feed-keywords)
			   :size size :name name :create create)))

(define-bknr-tag feed-form (&key feed-id)
  "Display a formular to edit a feed"
  (let ((feed (when feed-id (store-object-with-id feed-id))))
    (html ((:form :method "post")
	   (if feed
	       (html ((:input :type "hidden"
			      :name "feed-id"
			      :value (store-object-id feed)))
		     (:table (:tr (:td "name")
				  (:td ((:a :href (format nil "/feed/~a"
							  (feed-name feed)))
					(:princ-safe (feed-name feed)))))
			     (:tr (:td "url")
				  (:td ((:input :type "text" :size "50" :name "url"
						:value (feed-url feed)))))
			     (:tr (:td "refresh-interval")
				  (:td ((:input :type "text" :size "6" :name "refresh"
						:value (feed-refresh-interval feed)))))
			     (:tr (:td "type")
				  (:td (select-box "type" '(("rss091" "rss091")
							    ("rss10"  "rss10")
							    ("rss20"  "rss20")
							    ("atom"  "atom"))
						   :default (string-downcase
							     (symbol-name (feed-type feed))))))
			     (:tr (:td "encoding")
				  (:td (select-box "encoding" '(("iso-8859-1" "iso-8859-1")
								("utf-8"  "utf-8"))
						   :default (string-downcase
							     (symbol-name (feed-encoding feed))))))
			     (:tr (:td "last-update")
				  (:td (:princ-safe (format-date-time
						     (feed-last-updated feed)))))
			     (:tr (:td "keywords")
				  (:td
				   (loop for keyword in (mapcar #'string-downcase
								(feed-keywords feed))
					 do (html ((:a :href
						       (concatenate 'string
								    "/feed-keyword/" keyword))
						   (:princ #?"&nbsp;$(keyword)&nbsp;"))))))
			     (:tr (:td "add/rm keywords")
				 (:td (feed-keyword-choose-dialog :create t)))
			     (:tr ((:td :colspan "2")
				   (submit-button "save" "save")
				   (submit-button "add-keywords" "add keywords")
				   (submit-button "remove-keywords" "remove keywords")
				   (submit-button "update" "refresh feed")
				   (submit-button "delete" "delete feed")))))
	       (html (:table (:tr (:td "name")
				  (:td ((:input :type "text" :size "40" :name "name"))))
			     (:tr (:td "url")
				  (:td ((:input :type "text" :size "50" :name "url"))))
			     (:tr (:td "refresh-interval")
				  (:td ((:input :type "text" :size "6" :name "refresh"))))
			     (:tr (:td "type")
				  (:td (select-box "type" '(("rss091" "rss091")
							    ("rss10"  "rss10")
							    ("rss20"  "rss20")
							    ("atom"  "atom"))
						   :default "rss10")))
			     (:tr (:td "encoding")
				  (:td (select-box "encoding" '(("iso-8859-1" "iso-8859-1")
								("utf-8"  "utf-8"))
						   :default "iso-8859-1")))
			     (:tr (:td "keywords")
				 (:td (feed-keyword-choose-dialog :create t)))
			     (:tr ((:td :colspan "2")
				   (submit-button "create" "create"))))))))))

(define-bknr-tag rss-feed-page (link title grouped-items)
  (html ((:div :class "channel")
	 (:h2 ((:a :href link)
	       (:princ-safe title))))
	((:div :class "items")
	 (dolist (grouped-item grouped-items)
	   (html (:h2 (:princ-safe (format-date-time (car grouped-item)
						     :show-weekday t
						     :show-time nil
						     :show-seconds nil)))
		 (dolist (item (sort (cdr grouped-item) #'> :key #'rss-item-date))
		   (html ((:div :class "item")
			  (:h3 (if (rss-item-orig-feed item)
				   ;; XXX ieeks
				   (let* ((title (regex-replace-all "^.*?- " (rss-item-title item) ""))
					  (orig-channel (rss-feed-channel (rss-item-orig-feed item)))
					  (orig-title (rss-channel-title orig-channel))
					  (orig-link (rss-channel-link orig-channel)))
				     (if (and orig-title orig-link)
					 (html ((:a :href orig-link) (:princ-safe orig-title)))
					 (html (:princ orig-title)))
				     (html " - "
					   ((:a :href (rss-item-link item)) (:princ-safe title))))
				   (html ((:a :href (rss-item-link item))
					  (:princ (rss-item-title item))))))
			  (:p (:princ (rss-item-desc item)))))))))))

(define-bknr-tag feed (&key feed-id)
  "Display a feed"
  (let ((feed (store-object-with-id feed-id)))
    (let* ((rss-feed (feed-rss-feed feed))
	   (image (when rss-feed (rss-feed-image rss-feed))))
      (html (:h1 (:princ (feed-name feed)))
	    ((:a :href (format nil "/feed-rss/~a" (feed-name feed))) "rss") (:br)
	    "Last updated: " (:princ-safe
			      (format-date-time (feed-last-updated feed) :show-weekday t))
	    (:br)
	    (when (and image (rss-image-url image))
	      (html ((:img :src (rss-image-url image)
			   :alt (rss-image-title image)))))
	    "Keywords: "
	    (loop for keyword in (mapcar #'string-downcase (feed-keywords feed))
		  do (html ((:a :href (concatenate 'string "/feed-keyword/" keyword))
			    (:princ #?"&nbsp;$(keyword)&nbsp;"))))))))
