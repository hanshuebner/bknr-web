(in-package :bknr-user)

(enable-interpol-syntax)

(defclass edit-quizz-handler (edit-object-handler quizz-handler)
  ((require-user-flag :initform :quizz)))

(defmethod handle-object-form ((handler edit-quizz-handler) (action (eql nil)) (quizz (eql nil)))
  (with-bknr-page (:title #?"edit quizz")
    (:ul (dolist (quizz (all-quizz))
	   (html (:li (html-edit-link quizz)))))
    (:h2 "new quizz:")
    (quizz-form)))

(defmethod handle-object-form ((handler edit-quizz-handler) (action (eql nil)) quizz)
  (with-query-params (add-question add-mc-question)
    (let ((name (quizz-name quizz)))
      (with-bknr-page (:title #?"edit quizz: ${name}")
	(cond (add-question
	       (html
		(html-edit-link quizz)
		(:h2 "add a normal question:")
		(question-form)))
	      (add-mc-question
	       (html
		(html-edit-link quizz)
		(:h2 "add a multiple choice question:")
		(multiple-choice-question-form)))
	      (t
	       (html (:h2 "edit quizz:")
		     (quizz-form :quizz-id (store-object-id quizz))
		     (:h2 "edit questions:")
		     (:ul (dolist (question (quizz-questions quizz))
			    (html (:li (html-edit-link question)))))
		     (:p
		      ((:a :href (format nil "~a?add-question=1" (edit-object-url quizz)))
		       "add a normal question")
		      ((:a :href (format nil "~a?add-mc-question=1" (edit-object-url quizz)))
		       "add a multiple choice question")))))))))

(defmethod handle-object-form ((handler edit-quizz-handler)
			       (action (eql :create)) quizz)
  (with-query-params (name description)
    (if (and name description)
	(let* ((keywords (keywords-from-query-param-list (query-param-list "keyword")))
	       (quizz (make-instance 'quizz
                                     :name name
                                     :description description
                                     :keywords keywords)))
	  (redirect (edit-object-url quizz)))
	(redirect "/edit-quizz"))))

(defmethod handle-object-form ((handler edit-quizz-handler)
			       (action (eql :save)) quizz)
  (if quizz
      (with-query-params (description)
	(change-slot-values quizz 'description description)
	(redirect (edit-object-url quizz)))
      (redirect "/edit-quizz")))

(defmethod handle-object-form ((handler edit-quizz-handler)
			       (action (eql :add-keywords)) quizz)
  (if quizz
      (let ((keywords (keywords-from-query-param-list (query-param-list "keyword"))))
	(store-object-add-keywords quizz 'keywords keywords)
	(redirect (edit-object-url quizz)))
      (redirect "/edit-quizz")))

(defmethod handle-object-form ((handler edit-quizz-handler)
			       (action (eql :remove-keywords)) quizz)
  (if quizz
      (let ((keywords (keywords-from-query-param-list (query-param-list "keyword"))))
	(store-object-remove-keywords quizz 'keywords keywords)
	(redirect (edit-object-url quizz)))
      (redirect "/edit-quizz")))

(defmethod handle-object-form ((handler edit-quizz-handler)
			       (action (eql :delete)) quizz)
  (when quizz
;;; delete questions explicitely for now
    (dolist (question (quizz-questions quizz))
      (delete-object question))
    (delete-object quizz))
  (redirect "/edit-quizz"))

(defmethod handle-object-form ((handler edit-quizz-handler)
			       (action (eql :add-question)) quizz)
  (if quizz
      (with-query-params (name question)
	(let ((answers (keywords-from-query-param-list (query-param-list "answer"))))
	  (if (and name question answers)
	      (let ((question (make-instance 'question :name name
                                                       :question question
                                                       :answers answers
                                                       :quizz quizz)))
		(redirect (edit-object-url (question-quizz question))))
	      (redirect (edit-object-url quizz)))))
      (redirect "/edit-quizz")))

(defmethod handle-object-form ((handler edit-quizz-handler)
			       (action (eql :add-mc-question)) quizz)
  (if quizz
      (with-query-params (name question)
	(let ((answers (keywords-from-query-param-list (query-param-list "answer")))
	      (possible-answers (mapcar #'list
					(keywords-from-query-param-list
					 (query-param-list "possible-keyword"))
					(query-param-list "possible-answer"))))
	  (if (and name question answers possible-answers)
	      (let ((question (make-instance 'multiple-choice-question
                                             :name name
                                             :question question
                                             :answers answers
                                             :possible-answers possible-answers
                                             :quizz quizz)))
		(redirect (edit-object-url (question-quizz question))))
	      (redirect (edit-object-url quizz)))))
      (redirect "/edit-quizz")))

(defclass edit-question-handler (edit-object-handler question-handler)
  ((require-user-flag :initform :quizz)))

(defmethod handle-object-form ((handler edit-question-handler) (action (eql nil))
			       (question (eql nil)))
  (redirect "/edit-quizz"))

(defmethod handle-object-form ((handler edit-question-handler) (action (eql nil))
			       question)
  (let ((name (question-name question)))
    (with-bknr-page (:title #?"edit question: ${name}")
      (typecase question
	(multiple-choice-question
	 (multiple-choice-question-form :question-id (store-object-id question)))
	(question
	 (question-form :question-id (store-object-id question)))
	(t (error "No such question type"))))))

(defmethod handle-object-form ((handler edit-question-handler) (action (eql :delete))
			       question)
  (let ((quizz (question-quizz question)))
    (when question
      (delete-object question))
    (redirect (edit-object-url quizz))))

(defmethod handle-object-form ((handler edit-question-handler) (format-name (eql :save))
			       question-obj)
  (if question-obj
      (with-query-params (question)
	(let ((answers (keywords-from-query-param-list (query-param-list "answer")))
	      (possible-answers (mapcar #'list
					(keywords-from-query-param-list
					 (query-param-list "possible-keyword"))
					(query-param-list "possible-answer"))))
	  (typecase question-obj
	    (multiple-choice-question
	     (change-slot-values question-obj 'question question
				 'answers answers
				 'possible-answers possible-answers))
	    (question
	     (change-slot-values question-obj 'question question
				 'answers answers))
	    (t (error "Unknown question type"))))
	(redirect (edit-object-url question-obj)))
      (redirect "/edit-quizz")))