(in-package :bknr.sysparams)

(defvar *sysparams* (make-hash-table :test #'eql))

                                        ; xxx does not make use of datastore indices
(define-persistent-class sysparam ()
  ((name :read)
   (value :update)))

(defun sysparam (name)
  (let ((param (or (gethash name *sysparams*)
		   (find name (store-objects-with-class 'sysparam) :key #'sysparam-name))))
    (when param
      (setf (gethash name *sysparams*) param)
      (sysparam-value param))))

(defun set-sysparam (name value)
  (let ((param (or (find name (store-objects-with-class 'sysparam) :key #'sysparam-name)
		   (make-instance 'sysparam :name name))))
    (change-slot-values param 'value value)
    (setf (gethash name *sysparams*) param)))
