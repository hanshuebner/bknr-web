(in-package :bknr-user)

(define-bknr-tag quizz-keyword-choose-dialog
    (&key (size "4") (name "keyword") (create nil))
  (let ((size (or (parse-integer size :junk-allowed t) 1)))
    (keyword-choose-dialog (all-quizz-keywords)
			   :size size :name name :create create)))

(define-bknr-tag quizz-keyword-link (keyword)
  (let ((name (string-downcase (symbol-name keyword))))
    (html ((:a :href (format nil "/quizz-keyword/~A" name))
	   (:princ-safe name)))))

(define-bknr-tag quizz (&key quizz-id)
  (let ((quizz (find-store-object quizz-id :class 'quizz :query-function #'quizz-with-name)))
    (html ((:div :class "quizz")
	   ((:div :class "name") (:princ-safe (quizz-name quizz)))
	   (:p "keywords: "
	       (mapc #'quizz-keyword-link (quizz-keywords quizz)))
	   ((:table :class "scores")
	    (:tr (:td) (:td "Score") (:td "User"))
	    (loop for (user score) in (quizz-best-scores quizz)
		  for i from 1
		  do (html (:tr (:td (:princ-safe i) ". ")
				(:td (:princ-safe score))
				(:td (:princ-safe (user-login user)))))))
	   (:p ((:a :href (format nil "/quizz-take/~A"
				  (store-object-id quizz)))
		"Take the quizz"))))))

(define-bknr-tag quizz-form (&key quizz-id)
  (let ((quizz (find-store-object quizz-id :class 'quizz :query-function #'quizz-with-name)))
    (html ((:form :method "POST")
	   (if quizz
	       (html ((:input :type "hidden"
			      :name "quizz-id"
			      :value (store-object-id quizz)))
		     (:table (:tr (:td "name")
				  (:td (:princ-safe (quizz-name quizz))))
			     (:tr (:td "keywords")
				  (:td (dolist (keyword (quizz-keywords quizz))
					 (quizz-keyword-link keyword) (html " "))))
			     (:tr (:td "description")
				  (:td ((:textarea :name "description" :rows "15" :cols "60")
					(:princ (quizz-description quizz)))))
			     (:tr (:td "add/del keywords")
				  (:td (quizz-keyword-choose-dialog :create t)))
			     (:tr ((:td colspan 2)
				   (submit-button "save" "save")
				   (submit-button "add-keywords" "add keywords")
				   (submit-button "remove-keywords" "remove keywords")
				   (submit-button "delete" "delete")))))
	       (html (:table (:tr (:td "name")
				  (:td ((:input :type "text" :size "50" :name "name"))))
			     (:tr (:td "description")
				  (:td ((:textarea :name "description" :rows "15" :cols "60"))))
			     (:tr (:td "keywords")
				  (:td (quizz-keyword-choose-dialog :create t)))
			     (:tr ((:td :colspan 2)
				   (submit-button "create" "create"))))))))))

(define-bknr-tag standard-question-form (&key name question answers)
  (html
	(:tr (:td "name")
	     (:td (if name
		      (html (:princ-safe name))
		      (text-field "name" :value name))))
	(:tr (:td "question")
	     (:td (textarea-field "question" :value question)))
	(:tr (:td "answers")
	     (:td (:ul (dolist (answer answers)
			 (html (:li (text-field "answer" :size 20
						:value answer))))
		       (dotimes (i 3)
			 (html (:li (text-field "answer" :size 20)))))))))

(define-bknr-tag possible-answers-form (&optional possible-answers)
  (html (:tr (:td "possible answers")
	     (:td (:ul (loop for (keyword answer) in possible-answers
			     do (html (:li "answer: "
					   (text-field "possible-answer" :size 50 :value answer)
					   (:br) "keyword: " (text-field "possible-keyword" :size 20
									 :value keyword))))
		       (dotimes (i 3)
			 (html (:li "answer: " (text-field "possible-answer" :size 50)
				    (:br) "keyword: " (text-field "possible-keyword" :size 20)))))))))

(define-bknr-tag question-form (&key question-id)
  (let ((question (find-store-object question-id
				     :class 'question
				     :query-function #'question-with-name)))
    (html ((:form :method "POST") 
	   (if question
	       (html ((:input :type "hidden" :name "question-id"
			      :value (store-object-id question)))
		     (:table (:tr (:td "quizz")
				  (:td (html-edit-link (question-quizz question))))
			     (standard-question-form :name (question-name question)
						     :question (question-question question)
						     :answers (question-answers question))
			     (:tr ((:td :colspan 2)
				   (submit-button "save" "save")
				   (submit-button "delete" "delete")))))
	       (html (:table (standard-question-form)
			     (:tr ((:td :colspan 2)
				   (submit-button "add-question" "add-question"))))))))))

(define-bknr-tag multiple-choice-question-form (&key question-id)
  (let ((question (find-store-object question-id
				     :class 'multiple-choice-question
				     :query-function #'question-with-name)))
    (html ((:form :method "POST")
	   (if question
	       (html ((:input :type "hidden" :name "question-id"
			      :value (store-object-id question)))
		     (:table (standard-question-form :name (question-name question)
						     :question (question-question question)
						     :answers (question-answers question))
			     (possible-answers-form (multiple-choice-question-possible-answers
						     question))
			     (:tr ((:td :colspan 2)
				   (submit-button "save" "save")
				   (submit-button "delete" "delete")))))
	       (html (:table (standard-question-form)
			     (possible-answers-form)
			     (:tr ((:td :colspan 2)
				   (submit-button "add-mc-question" "add-mc-question"))))))))))

(defmethod question-tag ((question question))
  (html ((:div :class "quizz-question")
	 ((:input :type "hidden" :name "question-id" :value (store-object-id question)))
	 (:p (:princ-safe (question-question question)))
	 (:p ((:input :type "text" :size 40 :name "answer"))))))

(defmethod question-tag ((question multiple-choice-question))
  (html ((:div :class "quizz-question")
	 ((:input :type "hidden" :name "question-id" :value (store-object-id question)))
	 (:p (:princ-safe (question-question question)))
	 (:ul (loop for (keyword answer) in (multiple-choice-question-possible-answers question)
		    do (html (:li ((:input :type "radio"
					   :name "answer"
					   :value (string-downcase (symbol-name keyword)))
				   (:princ-safe answer)))))))))

(define-bknr-tag question (&key question-id)
  (let ((question (find-store-object question-id :class 'question
				     :query-function #'question-with-name)))
    (when question
      (question-tag question))))
	      
