(in-package :bknr-user)

(define-persistent-class question ()
  ((name :read
	 :index-type string-unique-index
	 :index-reader question-with-name
	 :index-values all-questions)

   (quizz :read :initform nil
	  :index-type hash-index
	  :index-reader quizz-questions)
   
   (question :update)
   (answers :update :initform nil)))

(defun answer-to-keyword (answer)
  (setf answer (regex-replace "^(\\s+)" answer ""))
  (setf answer (regex-replace "(\\s+)$" answer ""))
  (setf answer (regex-replace-all "([^a-zA-Z0-9\\s-]+)" answer ""))  
  (make-keyword-from-string (regex-replace-all "(\\s+)" answer "-")))

(defmethod answer-correct-p ((question question) answer)
  (let ((keyword (answer-to-keyword answer)))
    (member keyword (question-answers question))))

(define-persistent-class multiple-choice-question (question)
  ((possible-answers :update :initform nil)))

(define-persistent-class quizz ()
  ((name :update
	 :index-type string-unique-index
	 :index-reader quizz-with-name
	 :index-values all-quizz)
   
   (description :update :initform "")

   (keywords :update :transient t
	     :index-type hash-list-index
	     :index-reader get-keyword-quizz
	     :index-keys all-quizz-keywords)
   
   (best-scores :update :initform nil)))

