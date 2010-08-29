(in-package :bknr.text)

(defvar *word-scanner* (create-scanner "[a-z\\-]+" :case-insensitive-mode t))

(defun make-search-vector (hash)
  (let ((norm (sqrt (loop for val being the hash-values of hash
			  summing (* val val)))))
    (loop for key being the hash-keys of hash
	  do (setf (gethash key hash) (/ (gethash key hash) norm)))
    (cons norm hash)))

(defun search-vector-norm (vector)
  (car vector))

(defun search-vector-hash (vector)
  (cdr vector))

(defun search-vector-angle (v1 v2)
  (let* ((h1 (search-vector-hash v1))
	 (h2 (search-vector-hash v2)))
    (loop for key being the hash-keys of h1
	  for val2 = (gethash key h2)
	  when val2
	  sum (* (gethash key h1) val2))))

(defun string-to-search-vector (string)
  (let ((hash (make-hash-table :test #'equal)))
    (cl-ppcre:do-matches-as-strings (m *word-scanner* string)
      (let ((word (stem:stem (string-downcase m))))
	(incf-hash word hash)))
    (make-search-vector hash)))

(defun search-vector (search-vector objects &key (threshold 0.7) (key #'identity))
  (sort (remove-if-not #'(lambda (result)
			   (>= (car result) threshold))
		       (mapcar #'(lambda (o)
				   (cons (search-vector-angle search-vector
							      (funcall key o))
					 o))
			       objects))
	#'> :key #'car))

