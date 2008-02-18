(in-package :bknr.web)

(defvar *template-functions* (make-hash-table :test #'equal))

(defun register-tag-function (package-name name-string function-definition)
  (unless (gethash package-name *template-functions*)
    (setf (gethash package-name *template-functions*) (make-hash-table :test #'equal)))
  (setf (gethash name-string (gethash package-name *template-functions*))
	function-definition))

