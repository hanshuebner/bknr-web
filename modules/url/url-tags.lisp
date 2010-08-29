(in-package :bknr-user)

(enable-interpol-syntax)

(define-bknr-tag url-keyword-choose-dialog
    (&key (size "4") (name "keyword") (create nil) (values nil))
  (let ((size (or (parse-integer size :junk-allowed t) 1)))
    (keyword-choose-dialog (all-url-keywords)
			   :empty t
			   :size size :name name :create create :values values)))

(define-bknr-tag url-submitter-link (submitter)
  (let ((login (if (stringp submitter) submitter (if submitter (user-login submitter) "[unknown]"))))
    (html ((:a :href (format nil "/url-submitter/~A" login))
	   (:princ-safe login)))))

(define-bknr-tag url-keyword-link (keyword)
  (let ((name (string-downcase (symbol-name keyword))))
    (html ((:a :href (format nil "/url-keyword/~A" name))
	   (:princ-safe name)))))

(define-bknr-tag url-random-keywords (&key (count 10))
  (let ((keywords (subseq (randomize-list (copy-list (all-url-keywords)))
			  0 count)))
    (mapc #'url-keyword-link keywords)))

(define-bknr-tag url-submission (submission &key (full nil))
  (let ((url (url-submission-url submission)))
    (html ((:div :class "url")
	   ((:div :class "url-head")
	    ((:span :class "url-title")
	     ((:a :href (format nil "/url-redirect/~A"
				(store-object-id url)))
	      (:princ-safe (url-submission-title submission)))
	     (let ((cached-url (get-url-most-recent-cached-url (url-url url))))
	       (when cached-url
		 (html " " ((:a :href (format nil "/cached-url/~A"
					      (store-object-id cached-url)))
			    "(cached)")))))
	    
	    " / "
	    (if full
		(mapc #'url-keyword-link (url-submission-keywords submission))
		(let* ((keywords (url-keywords url))
		       (len (1- (length keywords)))
		       (keyword (first keywords)))
		  (url-keyword-link keyword)
		  (when (> len 0)
		    (html " " ((:a :href (object-url url))
			       (:princ-safe (format nil " ... and ~d ~a" len
						    (if (> len 1) "others" "other"))))))))
	    " / "
	    (if full
		(url-submitter-link (url-submission-submitter submission))
		(url-submitter-link (url-submission-submitter (url-latest-submission url))))
	    (unless full
	      (let ((len (1- (length (url-submissions url)))))
		(when (> len 0)
		  (html ((:a :href (object-url url))
			 (:princ-safe (format nil " ... and ~d ~a" len
					      (if (> len 1) "others" "other")))))))))
	   (when full
	     (html ((:div :class "url-date")
		    (:princ-safe (format-date-time (url-submission-date submission)
						   :show-weekday t)))))
	   (when (url-submission-description submission)
	     (html ((:div :class "url-description")
		    (:princ-safe (url-submission-description submission)))))))))

(define-bknr-tag url-submissions-page (grouped-submissions &key (full nil)
							   start-date
							   (url "/url-page"))
  (unless start-date
    (setf start-date (car (first grouped-submissions))))
  (next-days-list url :start start-date)
  (dolist (grouped-submission grouped-submissions)
    (html (:h2 (:princ-safe (format-date-time (car grouped-submission) :show-weekday t
					      :show-time nil
					      :show-seconds nil)))
	  (dolist (submission (sort (cdr grouped-submission) #'> :key #'url-submission-date))
	    (url-submission submission :full full))))
  (previous-days-list url :start start-date))

(define-bknr-tag url-page (grouped-submissions &key (full nil))
  (url-submissions-page grouped-submissions :full full))

(define-bknr-tag submit-url-form (&key url title description cache keywords redirect)
  (html ((:form :method "POST" :action "/submit-url")
	 (when redirect
	   (html ((:input :type "hidden" :name "redirect" :value "1"))))
	 (:table
	  (:tr (:td "keywords")
	       (:td (url-keyword-choose-dialog :create t :values keywords)))
	  (:tr (:td "url")
	       (:td (text-field "url" :value url)))
	  (:tr (:td "title")
	       (:td (text-field "title" :value title)))
	  (:tr (:td "description")
	       (:td (textarea-field "description" :value description)))
	  (when (user-has-flag *user* :cache)
	    (html (:tr ((:td))
		       (:td (checkbox-field "cache" "Cache URL" :checked cache)))))
	  (:tr ((:td :colspan 2)
		(submit-button "submit" "submit")))))))
