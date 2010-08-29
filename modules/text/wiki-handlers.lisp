(in-package :bknr.text)

(enable-interpol-syntax)

(defmethod edit-object-url ((article wiki-article))
  (format nil "/edit-wiki/~A" (wiki-article-keyword article)))

(defmethod object-url ((article wiki-article))
  (format nil "/wiki/~A" (wiki-article-keyword article)))

(defun wiki-keyword-url (keyword)
  (concatenate 'string "/wiki" "/" keyword))

(defclass wiki-handler (object-handler)
  ()
  (:default-initargs :object-class 'wiki-article :query-function #'wiki-article-with-keyword))

(defmethod handle-object ((handler wiki-handler) (article (eql nil)))
  (let ((keyword (parse-url)))
    (if (null keyword)
	(with-bknr-page (:title "all wiki keywords")
	  (:ul (dolist (keyword (sort (wiki-keywords) #'string<))
		 (html (:li ((:a :href (wiki-keyword-url keyword))
			     (:princ-safe keyword)))))))
	(redirect (concatenate 'string "/edit-wiki" "/"
			       (parse-url))))))

(defmethod handle-object ((handler wiki-handler) article)
  (let ((keyword (wiki-article-keyword article)))
    (with-bknr-page (:title #?"wiki article: ${keyword}")
      (wiki-article :id (store-object-id article)))))

(defclass edit-wiki-handler (edit-object-handler wiki-handler)
  ())

(defmethod authorized-p ((handler edit-wiki-handler))
  (not (anonymous-p (bknr-session-user))))

(defmethod handle-object-form ((handler edit-wiki-handler)
			       action (article (eql nil)))
  (with-bknr-page (:title "edit new wiki article")
    (wiki-article-form :keyword (parse-url))))

(defmethod handle-object-form ((handler edit-wiki-handler)
			       action article)
  (let ((keyword (wiki-article-keyword article)))
    (with-bknr-page (:title #?"edit wiki article blorg: ${keyword}")
      (:p (html-edit-link article))
      (wiki-article-form :id (store-object-id article)))))
			       
(defmethod handle-object-form ((handler edit-wiki-handler)
			       (action (eql :save)) article)
  (with-query-params (text comment)
    (let ((version (make-version (html-quote text)
				 :comment (html-quote comment)
				 :author (bknr-session-user)
				 :date (get-universal-time))))
      (if article
	  (article-add-version article version)
	  (setf article (make-instance 'wiki-article
                                       :subject (parse-url)
                                       :keyword (parse-url)
                                       :versions (list version))))
      (redirect (object-url article)))))

(define-bknr-webserver-module wiki
    ("/wiki" wiki-handler)
  ("/edit-wiki" edit-wiki-handler))
