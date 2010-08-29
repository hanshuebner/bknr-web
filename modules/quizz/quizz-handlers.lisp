(in-package :bknr-user)

(enable-interpol-syntax)

(defmethod edit-object-url ((quizz quizz))
  (format nil "/edit-quizz/~A"
	  (store-object-id quizz)))

(defmethod html-edit-link ((quizz quizz))
  (html ((:a :href (edit-object-url quizz))
	 (:princ-safe (quizz-name quizz)))))

(defmethod edit-object-url ((question question))
  (format nil "/edit-question/~A"
	  (store-object-id question)))

(defmethod html-edit-link ((question question))
  (html ((:a :href (edit-object-url question))
	 (:princ-safe (question-name question)))))

(defun quizz-list (quizz)
  (html (:ul (dolist (q quizz)
	       (html (:li ((:a :href (format nil "/quizz/~A"
					     (quizz-name q)))
			   (:princ-safe (quizz-name q)))))))))

(defclass quizz-handler (object-handler)
  ())

(defmethod object-handler-get-object ((handler quizz-handler))
  (find-store-object (parse-url) :class 'quizz :query-function #'quizz-with-name))

(defmethod handle-object ((handler quizz-handler) (quizz (eql nil)))
  (with-bknr-page (:title #?"bknr quizz: all quizz")
    (quizz-list (all-quizz))))

(defmethod handle-object ((handler quizz-handler) quizz)
  (let ((quizz-name (quizz-name quizz)))
    (with-bknr-page (:title #?"bknr quizz: ${quizz-name}")
      (quizz :quizz-id (store-object-id quizz)))))

(defclass quizz-keyword-handler (keyword-handler)
  ())

(defmethod handle-object ((handler quizz-keyword-handler)
			  (keyword (eql nil)))
  (with-bknr-page (:title "all-quizz-keywords")
    (:ul (dolist (keyword (all-quizz-keywords))
	   (html (:ul (quizz-keyword-link keyword)))))))

(defmethod handle-object ((handler quizz-keyword-handler) keyword)
  (with-bknr-page (:title "quizz keyword: ${keyword}")
    (quizz-list (get-keyword-quizz keyword))))

(defclass question-handler (object-handler)
  ())

(defmethod object-handler-get-object ((handler question-handler))
  (find-store-object (parse-url) :class 'question :query-function #'question-with-name))

(defclass quizz-take-handler (edit-object-handler quizz-handler)
  ())

(defmethod authorized-p ((handler quizz-take-handler))
  t)

(defmethod handle-object-form ((handler quizz-take-handler)
			       (action (eql nil))
			       (quizz (eql nil)))
  (with-bknr-page (:title #?"all quizz")
    (:ul (dolist (quizz (all-quizz))
	   (html (:li ((:a :href (format nil "/quizz-take/~A"
					 (store-object-id quizz)))
		       (:princ-safe (quizz-name quizz)))))))))

(defmethod handle-object-form ((handler quizz-take-handler) (action (eql nil))
			       quizz)
  (let ((quizz-name (quizz-name quizz)))
    (with-bknr-page (:title #?"take the quizz: ${quizz-name}")
      ((:form :method "POST")
       (mapc #'(lambda (question)
		 (question :question-id (store-object-id question)))
	     (quizz-questions quizz))
       (submit-button "results" "see your results")))))

(defmethod handle-object-form ((handler quizz-take-handler)
			       (action (eql :results))
			       quizz)
  (if quizz
      (let* ((ids (query-param-list "question-id"))
	     (answers (query-param-list "answer"))
	     (questions (mapcar #'(lambda (id) (find-store-object id :class 'question))
				ids))
	     (score 0)
	     correct-answers
	     wrong-answers
	     (quizz-name (quizz-name quizz)))
	(with-bknr-page (:title #?"results for: ${quizz-name}")
	  (loop for question in questions
		for answer in answers
		when (answer-correct-p question answer)
		do (incf score)
		(push (list answer question) correct-answers)
		else do (push (list answer question) wrong-answers))
	  (:h2 "Score:")
	  (:p "Your score is " (:princ-safe score) ".")
	  (:h2 "Correct answers:")
	  (:ul (loop for (answer question) in (nreverse correct-answers)
		     do (html (:li (:princ-safe (question-name question)) ": "
				   (:princ-safe answer)))))
	  (:h2 "Wrong answers:")
	  (:ul (loop for (answer question) in (nreverse wrong-answers)
		     do (html (:li (:princ-safe (question-name question)) ": "
				   (:princ-safe answer) " is wrong. "
				   (:princ-safe (format nil "~a" (question-answers question)))
				   " would have been correct."))))))
      (redirect "/quizz-take")))

