(in-package :bknr.text)

(enable-interpol-syntax)

(defun article-blog-headline (article)
  (html ((:div :class "headline")
	 ((:span :class "subject")
          (:princ-safe (or (article-subject article) "")))
	 " "
	 ((:span :class "date")
	  (:princ (format-date-time (article-time article)
				    :show-weekday t :show-year nil :show-seconds nil)))
	 " "
	 (when (article-author article)
	   (html ((:span :class "author") (:princ-safe (user-login (article-author article)))))))))

(define-bknr-tag article (&key id (class "article"))
  (let ((article (when id (find-store-object id :class 'article))))
    (html ((:div :class class)
	   (unless (string-equal (article-subject article) "")
	     (article-blog-headline article))
	   (when (article-text article)
	     (html
              ((:div :class "text")
               (:princ (article-text article)))))))
    (unless (user-has-flag (bknr-session-user) :locked)
      (user-has-read-article article (bknr-session-user)))))

(define-bknr-tag wiki-article (&key id (edit-url "/edit-wiki"))
  (let* ((article (find-store-object id
				    :class 'wiki-article
				    :query-function #'wiki-article-with-keyword))
	 (version (when article (versioned-article-newest-version article))))
    (if version
	(html (:h2 (:princ-safe (wiki-article-keyword article)))
	      ((:p :class "wiki")
	       (:princ (let ((text (version-text version)))
			 (setf text (make-wiki-hrefs (version-text version)))
			 (setf text (text-to-html text))
		(setf text (regex-replace-all "[\\r\\n]" text "<br />"))
		text)))
	       ((:div :class "wiki-bottom")
		"Last edit on "
		((:span :class "date")
		 (:princ (format-date-time (version-date version) :show-weekday t :show-seconds nil)))
		" by "
		((:span :class "author") (:princ-safe (when (version-author version)
							(user-login (version-author version)))))
		((:span :class "edit") " (" ((:a :href (format nil "~a/~a" edit-url
							       (wiki-article-keyword article)))
					     "edit") ")")))
	(html (:h2 "No such article")
	      ((:a :href (format nil "~A/~A" edit-url id)) "edit " (:princ-safe id))))))

(define-bknr-tag article-form (&key id)
  (let ((article (when id (find-store-object id :class 'article))))
    (html
     ((:script :language "JavaScript") "initEditor();")
     ((:form :method "post")
      (when article
        (html ((:input :type "hidden" :name "article-id" :value (store-object-id article)))))
      (:table (:tr (:td "subject")
                   (:td ((:input :type "text" :size "50" :name "subject"
                                 :value (if article
                                            (article-subject article)
                                            "")))))
              (:tr (:td "message")
                   (:td ((:textarea :name "text" :rows "15" :cols "60")
                         (if article
                             (html (:princ (article-text article)))
                             (html " ")))))
              (:tr (:td (submit-button "save" "save"))))))))

(define-bknr-tag wiki-article-form (&key id keyword)
  (let ((article (when id (find-store-object id :class 'wiki-article :query-function
					     #'wiki-article-with-keyword))))
    (html ((:form :method "post")
	   (when article
	     (html ((:input :type "hidden" :name "article-id" :value (store-object-id article)))))
	   (:table (:tr (:td "subject")
			(:td (:princ-safe
			      (if article (wiki-article-keyword article) keyword))))
		   (:tr (:td "message")
			(:td (textarea-field "text" :cols 80 :rows 60
			       :value (when article
					(version-text
					 (versioned-article-newest-version
					  article))))))
		   (when article
		     (html (:tr (:td "comment")
				(:td (textarea-field "comment" :rows 3)))))
		   (:tr (:td (submit-button "save" "save"))))))))

(defparameter *snippet-layouts* '((:textbox1 . "textbox_1")
				  (:textbox2 . "textbox_2")
				  (:textbox3 . "textbox_3")))

(define-bknr-tag snippet (&key id)
  (let ((snippet (find-store-object id :class 'snippet)))
    (when snippet
      (let* ((layouts (mapcar #'car *snippet-layouts*))
	     (layout (or (snippet-layout snippet)
			 (random-elt layouts))))
	(pushnew snippet (request-variable :shown-snippets))
	(article :id id :class (cdr (assoc layout *snippet-layouts*)))))))

(define-bknr-tag random-snippet (&key keywords)
  (let* ((keys (mapcar #'make-keyword-from-string (split "," keywords)))
	 (snippets (set-difference (get-current-snippets :keys keys)
				   (request-variable :shown-snippets)))
	 (snippet (random-elt snippets)))
    (when snippet
      (snippet :id (store-object-id snippet)))))

(define-bknr-tag snippet-keyword-choose-dialog (&key (name "keyword") (create nil))
  (keyword-choose-dialog (all-article-keywords)
			 :name name :size 4 :create create))

(define-bknr-tag snippet-form (&key id)
  (let* ((snippet (when id (find-store-object id :class 'snippet)))
	 (layout (when snippet (snippet-layout snippet))))
    (html ((:form :method "post")
	   (:table (:tr (:td "subject")
			(:td ((:input :type "text" :size "50" :name "subject"
				      :value (if snippet (article-subject snippet) "")))))
		   (when snippet
		     (html ((:input :type "hidden" :name "snippet-id" :value (store-object-id snippet)))
			   (:tr (:td "keywords")
				(:td (dolist (keyword (keywords-article-keywords snippet))
				       (html (:princ-safe (string-downcase (symbol-name keyword))) " "))))
			   (:tr (:td "created")
				(:td (:princ-safe (format-date-time (article-time snippet)
								    :show-weekday t))))))
		   (:tr (:td "expires")
			(:td (date-field "expiration" :date (if snippet
								(snippet-expires snippet)
								(get-universal-time)))))
		   (:tr (:td "layout")
			(:td (keyword-choose-dialog (mapcar #'car *snippet-layouts*)
						    :name "layout" :size 1
						    :values (when layout (list layout))
						    :empty t)))
		   (:tr (:td "keywords")
			(:td (snippet-keyword-choose-dialog :name "keyword" :create t)))
		   (:tr (:td "message")
			(:td ((:textarea :name "text" :rows "15" :cols "60")
			      (when snippet (html (:princ-safe (article-text snippet)))))))
		   (:tr ((:td :colspan 2)
			 (if snippet
			     (progn 
			       (submit-button "save" "save")
			       (submit-button "add-keywords" "add-keywords")
			       (submit-button "remove-keywords" "remove-keywords")
			       (submit-button "delete" "delete"))
			     (submit-button "create" "create")))))))))

(define-bknr-tag blog (&key name suppress-title)
  "Display blog including headlines"
  (let ((blog (find-store-object name :class 'blog :query-function #'blog-with-name)))
    (if blog
	(progn (unless suppress-title
		 (html (:h3 (:princ-safe (blog-name blog)))
		       (html (when (admin-p (bknr-session-user))
			       (html-edit-link blog))
			     ((:a :href (format nil "/rss/~a" name))
			      (:princ "&nbsp;rss&nbsp;")))))
	       (loop for article in (sort (copy-list (blog-articles blog))
					  #'> :key #'article-time)
		     do (if (and (not (equal "anonymous" (user-login (bknr-session-user))))
				 (article-read article (bknr-session-user)))
			    (html ((:div :class "textbox_3")
				   ((:a :href (object-url article))
				    (article-blog-headline article))))
			    (article :id (store-object-id article)))))
	(html "Could not find the blog"))))

(define-bknr-tag blog-page (blog grouped-articles &key start-date)
  (let ((url (format nil "/blog/~A" (blog-name blog))))
    (html (:h3 (:princ-safe (blog-name blog)))
	  (when (admin-p (bknr-session-user))
	    (html-edit-link blog))
	  ((:a :href (format nil "/rss/~A" (blog-name blog)))
	   "rss")
	  ((:a :href (format nil "/search-blog/~A" (blog-name blog)))
	   "search")
	  (unless start-date
	    (setf start-date (car (first grouped-articles))))
	  (next-days-list url :start start-date)
	  (dolist (grouped-article grouped-articles)
	    (html (:h2 (:princ-safe (format-date-time (car grouped-article)
						      :show-weekday t
						      :show-time nil
						      :show-seconds nil)))
		  (dolist (article (sort (cdr grouped-article) #'> :key #'article-time))
		    (if (and (not (equal "anonymous" (user-login (bknr-session-user))))
			     (article-read article (bknr-session-user)))
			(html ((:div :class "textbox_3")
			       ((:a :href (object-url article))
				(article-blog-headline article))))
			(article :id (store-object-id article)))))))))

(define-bknr-tag blog-search-form (&key title)
  (html ((:div :class "search-images")
	 (when title
	   (html (:h3 (:princ-safe title))))
	 ((:form :method "POST")
	  (text-field "search" :value (query-param "search"))
	  (submit-button "search" "Search!")))))

(define-bknr-tag blog-search-results ()
  (let* ((page (parse-integer (or (query-param "page") "0")))
	 (num-pages (ceiling (/ (length (session-value :blog-search-results)) 10)))
	 (results (subseq* (session-value :blog-search-results)
                           (* page 10)
                           (* (1+ page) 10))))
    (when results
      (html (:h3 "Results for \"" (:princ-safe (session-value :blog-search)) "\":"))
      (dotimes (i num-pages)
	(html "&nbsp;"
	      (if (= i page)
		  (html (:princ-safe i))
		  (html ((:a :href (format nil "~A?page=~A"
					   (script-name*) i))
			 (:princ-safe i))))
	      "&nbsp;"))
      (loop for result in results
	    do (article :id (store-object-id (cdr result)))))))

(define-bknr-tag wiki-latest-articles (&key (length "10") (url "/wiki"))
  (let ((articles (get-wiki-latest-revisions :length (or (parse-integer length :junk-allowed t) 10))))
    (html ((:ul :class "wiki-latest")
	   (dolist (article articles)
	     (let ((version (versioned-article-newest-version article)))
	       (html (:li ((:a :href (format nil "~a/~a" url (wiki-article-keyword article)))
			   (:princ-safe (wiki-article-keyword article))) " by "
			   (:princ-safe (when (version-author version)
					  (user-login (version-author version)))) " on "
			   (:princ-safe (format-date-time (version-date version)
							  :show-weekday t))))))))))
