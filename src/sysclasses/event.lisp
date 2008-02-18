(in-package :bknr.events)

;; event and subclasses

(define-persistent-class event ()
  ((time :read))
  (:documentation "generic event"))

(defclass event-handler ()
  ())

(defmethod initialize-instance :after ((handler event-handler) &key)
  (register-event-handler handler))

(defgeneric handle-event (event-handler event)
  (:method-combination progn))

(defmethod handle-event progn ((event-handler event-handler) event)
	   "empty method"
	   nil)

(defvar *event-creation-hooks* nil)
(defvar *hook-lock*
  (mp-make-lock "Event Creation Hook Lock"))

(defun register-event-handler (event-handler)
  (mp-with-lock-held (*hook-lock*)
    (push event-handler *event-creation-hooks*)))

(defun deregister-event-handler (event-handler)
  (mp-with-lock-held (*hook-lock*)
    (setq *event-creation-hooks*
      (delete event-handler *event-creation-hooks* :test #'equal))))

(defgeneric make-one-liner (event))

(defmethod make-one-liner ((event event))
  (format nil "~a ~a" (event-class-name event) (event-argument event)))

(defgeneric print-as-html (event stream))

(defmethod print-as-html ((event event) stream)
  (html-stream stream
	       (:princ-safe (format-date-time (event-time event) :show-year nil))
	       "&nbsp;["
	       (:princ-safe (class-of event))
	       "]"))

(defgeneric event-argument (event))

(defmethod event-argument ((event event))
  "")

(defgeneric event-class-name (event))

(defmethod event-class-name ((event event))
  (regex-replace-all "-event$" (symbol-name (class-name (class-of event))) ""))

(defgeneric as-two-panel-lines (event))

(defun cut-string (string length)
  (if (> (length string) length)
      (subseq string 0 length)
    string))

(defmethod as-two-panel-lines ((event event))
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (event-time event))
    (declare (ignore sec day month year))
    (format nil "~15a~2d:~2,'0d~20a"
	    (format nil "[~a]"
		    (cut-string (event-class-name event) 13))
	    hour min
	    (cut-string (event-argument event) 20))))

(defgeneric as-xml (event)
  (:documentation "Generic function which returns the given event an XML form suitable for generate-xml-with-stream"))

(defmacro event-xml (event &body body)
  ``((:event :type ,(format nil "~a" (event-class-name ,event))
	     :class ,(format nil "~a" (event-class-name ,event)) ; name alias to appease flash
	     :date ,(format-date-time (event-time ,event) :show-time nil :show-year :short)
	     :time ,(format-date-time (event-time ,event) :show-date nil)
	     ,,@body)))

(defmethod as-xml ((event event))
  (event-xml event))

(defmethod print-object ((event event) stream)
  (format stream "#<~a at ~a>" (class-name (class-of event)) (format-date-time (event-time event)))
  event)

(defun find-events (&key (from (error "missing :from argument to find-events"))
			 (to (get-universal-time))
			 count
			 class
			 (include-subclasses t))
  (when (and count (zerop count)) (return-from find-events nil))
  (when (and class (symbolp class))
    (setf class (find-class class)))
  (loop for event in (class-instances 'event)
	when (and (> to (event-time event))
		  (< from (event-time event))
		  (or (not class)
		      (if include-subclasses
			  (subtypep (type-of event) (class-name class))
			  (equal (class-of event) class))))
	collect event into results-list
	when (and count (= count (length results-list)))
	do (return results-list)
	finally (return results-list)))
(defun quote-xml-attribute-value (string)
  ;; return the given string quoted according to xml quoting rules
  (unless (stringp string)
    (setf string (format nil "~a" string)))
  (let ((retval (list)))
    (do* ((i 0 (1+ i))
	  (start i)
	  (end (length string)))
	((>= i end)
	 (when (< start i)
	   (push (subseq string start i) retval)))
      (let ((cvt (case (schar string i)
		   (#\< "&lt;")
		   (#\> "&gt;")
		   (#\& "&amp;")
		   (#\" "&quot;"))))
	(when cvt
					; must do a conversion, emit previous chars first
	  (when (< start i)
	    (push (subseq string start i) retval))
	  (push cvt retval)
	  (setq start (1+ i)))))
    (apply #'concatenate (cons 'string (nreverse retval)))))

(defun lisp-name-to-javascript (lisp-name)
  (regex-replace-all "-(.)" lisp-name #'(lambda (target-string start end match-start match-end &rest args)
					  (declare (ignore start end args))
					  (string-capitalize (subseq target-string (1+ match-start) match-end)))))

(defun make-javascript-event-handler-skeleton (event)
  (let ((attributes (cdar (as-xml event))))
    (format t "
function ~a(~a) {
  // placeholder
}
"
	    (lisp-name-to-javascript (event-class-name event))
	    (apply #'concatenate 'string
		   (loop
		       for rest on attributes by #'cddr
		       for keyword = (car rest)
		       unless (find keyword '(:type :class))
		       collect (symbol-name keyword)
		       and
		       when (cddr rest)
		       collect ", ")))))

(defmacro generate-event-xml (event &rest attributes)
  "Generate XML form of the given event.  The common fields are generated by the event-xml function, the remaining 
fields are provided as attributes in the form ':key1 value1 :key2 value2...'.  Proper quoting of the data is
ensured by generating calls to quote-xml-attribute-value for every value provided."
  `(event-xml ,event
	      ,@(loop
		    for keyword in attributes by #'cddr
		    for value in (cdr attributes) by #'cddr
		    collect keyword
		    collect `(quote-xml-attribute-value ,value))))

(defun event-backlog (&key (hours 12) latest-last)
  (sort (remove-if #'(lambda (event) (eq (type-of event) 'image-snapshot-event))
		   (find-events :from (- (get-universal-time) (* 3600 hours))))
	(if latest-last
	    #'(lambda (a b) (< (event-time a) (event-time b)))
	  #'(lambda (a b) (> (event-time a) (event-time b))))))

(defun make-event (class &rest args)
  (mp-with-lock-held (*hook-lock*)
    ;; xxx fixme consing to new-hooks
    (let (new-hooks
	  (event (apply #'make-object class :time (get-universal-time) args)))
      (dolist (event-creation-handler *event-creation-hooks*)
	(handler-case
	    (progn
	      (handle-event event-creation-handler event)
	      (push event-creation-handler new-hooks))
	  (error (e)
	    (format t ";; error during make-event ignored: ~a~%" e))))
      (setf *event-creation-hooks* (reverse new-hooks))
      event)))
