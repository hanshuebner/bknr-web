(in-package :bknr.text)

(enable-interpol-syntax)

(defmethod edit-object-url ((paste paste))
  (format nil "/paste/~A" (store-object-id paste)))

(defmethod html-edit-link ((paste paste))
  (html
   ((:a :href (edit-object-url paste))
    (:princ (format nil " ~a " (article-subject paste))))))

(defclass paste-handler (edit-object-handler)
  ())

(defmethod authorized-p ((handler paste-handler))
  t)

(defmethod object-handler-get-object ((handler paste-handler))
  (find-store-object (parse-url) :class 'annotated-article))

(defmethod handle-object-form ((handler paste-handler)
			       action (foo (eql nil)))
  (with-bknr-page (:title "paste")
    (:h2 "new paste:")
    (paste-form)
    (:h2 "old pastes:")
    (:ul (dolist (paste (sort (all-pastes) #'> :key #'article-time))
	   (html (:li (html-edit-link paste)))))))

(defmethod handle-object-form ((handler paste-handler)
			       action paste)
  (let ((subject (article-subject paste)))
    (with-bknr-page (:title #?"paste: ${subject}")
      (paste paste)
      (:h2 "annotate:")
      (paste-annotate-form))))

(defmethod handle-object-form ((handler paste-handler)
			       (action (eql :create))
			       foo)
  (with-query-params (subject text lisp)
    (if (and subject text)
	(let ((paste (make-instance 'paste
                                    :author (bknr-session-user)
                                    :subject subject
                                    :time (get-universal-time)
                                    :text text
                                    :keywords (when lisp '(:lisp))
                                    :expires (get-universal-time))))
	  (if paste
	      (redirect (edit-object-url paste))
	      (redirect "/paste")))
	(redirect "/paste"))))

(defmethod handle-object-form ((handler paste-handler)
			       (action (eql :annotate))
			       paste)
  (if paste
      (with-query-params (text lisp)
	(let ((annotation (make-instance 'keywords-article
                                         :author (bknr-session-user)
                                         :subject ""
                                         :time (get-universal-time)
                                         :text text
                                         :keywords (when lisp '(:lisp)))))
	  (if annotation
	      (annotate-article paste annotation))
	  (redirect (edit-object-url paste))))
      (redirect "/paste")))
				