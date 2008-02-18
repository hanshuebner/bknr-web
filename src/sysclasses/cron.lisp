(in-package :bknr.cron)

(defun minute-p (minute)
  (and (numberp minute)
       (>= minute 0)
       (< minute 60)))

(defun hour-p (hour)
  (and (numberp hour)
       (>= hour 0)
       (< hour 24)))

(defparameter *day-list* '(:monday :tuesday :wednesday :thursday :friday :saturday :sunday))

(defun day-p (day)
  (or (and (numberp day)
	   (>= day 1)
	   (<= day 7))
      (and (symbolp day)
	   (member day *day-list*))))

(defun day-to-number (day)
  (if (numberp day)
      day
      (let ((num (position day *day-list*)))
	(if num
	    (1+ num)
	    (error "Could not find day in day-list")))))

(defparameter *month-list* '(:january :february :march :april :may :june :july
			    :august :september :october :november :december))

(defun month-p (month)
  (or (and (numberp month)
	   (>= month 1)
	   (<= month 12))
      (and (symbolp month)
	   (member month *month-list*))))

(defun month-to-number (month)
  (if (numberp month)
      month
      (let ((num (position month *month-list*)))
	(if num
	    (1+ num)
	    (error "Could not find month in month-list")))))

(defun parse-cron-spec (cronspec &key test every-list (convert #'identity))
  (sort (copy-list (cond ((funcall test cronspec)
			  (list (funcall convert cronspec)))
			 ((listp cronspec)
			  (if (every #'minute-p cronspec)
			      (mapcar convert cronspec)
			      (error "Not every element of ~a is of the valid type" cronspec)))
			 ((eq cronspec :every) every-list)
			 (t (error "Unknown cronspec: ~a" cronspec))))
	#'<))

(defun normalize-minute-spec (cronspec)
  (parse-cron-spec cronspec :test #'minute-p :every-list (genlist 0 59)))

(defun normalize-hour-spec (cronspec)
  (parse-cron-spec cronspec :test #'hour-p :every-list (genlist 0 23)))

(defun normalize-day-spec (cronspec)
  (parse-cron-spec cronspec
		   :test #'day-p
		   :every-list (genlist 0 6)
		   :convert #'day-to-number))

(defun normalize-month-spec (cronspec)
  (parse-cron-spec cronspec
		   :test #'month-p
		   :every-list (genlist 1 12)
		   :convert #'month-to-number))

(define-persistent-class cron-job ()
  ((name :read
	 :index-type unique-index :index-initargs (:test #'equal)
	 :index-reader cron-job-with-name)
   (minute :update :initform :every)
   (minute-normal :update :transient t)
   (hour :update :initform :every)
   (hour-normal :update :transient t)   
   (day :update :initform :every)
   (day-normal :update :transient t)
   (month :update :initform :every)
   (month-normal :update :transient t)   
   (job :update)))

(defmethod initialize-transient-instance ((job cron-job))
  (setf (cron-job-minute-normal job) (normalize-minute-spec (cron-job-minute job))
	(cron-job-hour-normal job) (normalize-hour-spec (cron-job-hour job))
	(cron-job-day-normal job) (normalize-day-spec (cron-job-day job))
	(cron-job-month-normal job) (normalize-month-spec (cron-job-month job))))

(defmethod print-object ((object cron-job) stream)
  (format stream "#<~a ID: ~a RUN-AT: ~a,~a,~a NAME: ~s>"
	  (class-name (class-of object))
	  (store-object-id object)
	  (cron-job-day object)
	  (cron-job-hour object)
	  (cron-job-minute object)
	  (cron-job-name object))
  object)

(defun make-cron-job (name job &optional (minute :every) (hour :every) (day :every) (month :every))
  (unless (find (type-of job) '(list symbol cons))
    (error "invalid type of job argument, must be either list of forms or a function symbol"))
  (when (cron-job-with-name name)
    (error "can't create cron job ~S - duplicate name" name))
  (make-object 'cron-job :name name
	       :job job
	       :minute minute :hour hour :day day :month month))

(defun all-cron-jobs ()
  (store-objects-with-class 'cron-job))

(defmethod cron-job-should-execute-p ((job cron-job))
  (multiple-value-bind (usec umin uhour uday umonth uyear udate)
      (decode-universal-time (get-universal-time))
    (declare (ignore usec uyear uday))
    (every #'member (list umin uhour udate umonth)
	   (list (cron-job-minute-normal job)
		 (cron-job-hour-normal job)
		 (cron-job-day-normal job)
		 (cron-job-month-normal job)))))

(defclass cron-job-executor (bknr-actor)
  ((cron-job :initarg :cron-job)))

(defmethod run-function ((executor cron-job-executor))
  (with-slots (cron-job) executor
    (handler-case
	(with-slots (job) cron-job
	  (typecase job
	    (list (eval job))
	    (symbol (funcall (symbol-function job)))
	    (t (error "Unknown cron-job type ~A in cron-job ~A" (type-of job) cron-job))))
      (error (e)
	(warn "Error ~A ignored while executing cron job ~A" e cron-job)))))

(defclass cron-actor (bknr-actor)
  ())

(defun time-until-full-minute (time)
  (let ((seconds (decode-universal-time time)))
    (- 60 seconds)))

(defmethod run-function ((actor cron-actor))
  (loop
   (sleep (time-until-full-minute (get-universal-time)))
   (format t "; cron executing jobs at ~A~%" (format-date-time))
   (dolist (job (remove-if-not #'cron-job-should-execute-p (all-cron-jobs)))
     (actor-start (make-instance 'cron-job-executor :cron-job job)))))

(defvar *cron* nil)

(defun start-cron ()
  (unless *cron*
    (setq *cron* (make-instance 'cron-actor)))
  (when (actor-running-p *cron*)
    (actor-stop *cron*))
  (actor-start *cron*))