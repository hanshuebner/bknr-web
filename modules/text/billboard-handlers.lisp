(in-package :bknr.text)

(enable-interpol-syntax)

(defmethod article-as-html ((article article) &key (print-header t))
  (when print-header
    (html ((:p :class "articleHeader")
	   "date: " (:princ (format-date-time (article-time article)
					      :show-weekday t :show-year nil :show-seconds nil))
	   (when (article-author article)
	     (html 
	      :br
	      "from: " (html-link (article-author article))))
	   (when (article-subject article)
	     (html
	      :br
	      "subject: " (:princ-safe (article-subject article)))))))
  (html
   ((:p :class "articleText") (:princ (article-html-text article)))))

(defun list-billboards-page ()
  (let ((may-edit (admin-p (bknr-session-user))))
    (with-bknr-page (:title "billboards")
      (html
       ((:form :method "post" :action (script-name*))
	((:table :width "640")
	 (:tr (:th "name")
	      (:th "new" :br "msgs")
	      (:th "description")
	      (when may-edit
		(html (:th "action"))))
	 (loop
            for billboard in (sort (all-billboards)
                                   #'string< :key #'billboard-name)
            ;; ensure billboard-description
            do (unless (billboard-description billboard)
                 (billboard-make-empty-description (store-object-id billboard)
                                                   (store-object-id *user*)))
            do (html
                (:tr (:td ((:a :href (format nil "/billboard/~a" (billboard-name billboard)))
                           (:princ-safe (billboard-name billboard))))
                     (:td (when (billboard-articles billboard :new-for-user *user*)
                            (length (billboard-articles billboard :new-for-user *user*))))
                     (:td (:princ-safe (article-subject (billboard-description billboard))))
                     (when may-edit
                       (html
                        (:td
                         ((:a :href (format nil "/edit-article/~a"
                                            (store-object-id (billboard-description billboard))))
                          "&nbsp;edit&nbsp;")))))))))))))

;; xxx using old store api
(defun billboard-page ()
  (let ((billboard (parse-url)))
    (with-query-params (new show-all delete)
      (let ((may-edit (admin-p (bknr-session-user))))
	(setf billboard (find-billboard (or billboard *default-billboard*)))
	(if delete
	    (let ((article (store-object-with-id delete)))
	      (billboard-delete-article article billboard)
	      (with-bknr-page (:title "article deleted")
		(html "the article has been deleted")))
	    (if (and new may-edit)
		(let ((article (make-instance 'article
                                              :author (bknr-session-user))))
		  (billboard-add-article billboard article)
		  (redirect (format nil "/edit-article/~a" (store-object-id article))))
		(with-bknr-page (:title #?"billboard: $((billboard-name billboard))")
		  (when (billboard-always-show-all billboard)
		    (setf show-all t))
		  ((:form :method "post")
		   ((:input :type "hidden" :name "billboard" :value (billboard-name billboard)))
		   ((:table :width "640")
		    (loop
                       with shown
                       for article in (billboard-articles billboard)
                       do (when (or show-all
                                    (not (article-read article (bknr-session-user))))
                            (setf shown t)
                            (html
                             (:tr (:td "date")
                                  (:td (:princ (format-date-time (article-time article)
                                                                 :show-weekday t :show-year nil :show-seconds nil))))
                             (when (article-author article)
                               (html
                                (:tr
                                 (:td "from")
                                 (:td (html-link (article-author article))))))
                             (:tr (:td "subject")
                                  (:td (:princ-safe (article-subject article))))
                             (:tr (:td "&nbsp;"))
                             (:tr (:td) ((:td :colspan "2") (article-as-html article :print-header nil))))
                            (when may-edit
                              (html
                               (:tr (:td "action")
                                    (:td
                                     ((:a :href (format nil "/billboard/~a&delete=~a"
                                                        (billboard-name billboard) (store-object-id article)))
                                      "&nbsp;delete&nbsp;")
                                     " "
                                     ((:a :href (format nil "/edit-article/~a"
                                                        (store-object-id article))) "&nbsp;edit&nbsp;")))))
                            (html (:tr ((:td :colspan "2") :hr))))
                       finally (unless shown (html ((:td :colspan "3") 
                                                    (format *html-stream* "(no ~[new ~;~]messages)" (if (billboard-articles billboard) 1 0)))))))
		   (unless (billboard-always-show-all billboard)
		     (html
		      ((:input :type "submit" :name "show-all" :value "show-all"))))
		   (when (admin-p (bknr-session-user))
		     (html
		      ((:input :type "submit" :name "new" :value "new"))))))))))))