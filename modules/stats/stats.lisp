(in-package :bknr.stats)

;; The stats module is no longer supported because it uses a
;; standard-class definition and tries to make that persistent, which
;; does not work well with recent store changes.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass log-stats ()
    ((sessions :reader log-stats-sessions
	       :initarg :sessions
	       :initform 0)
     (hits :reader log-stats-hits
	   :initarg :hits
	   :initform 0)
     (hits-per-hour :reader log-stats-hits-per-hour
		    :initarg :hits-per-hour
		    :initform nil)      ; '(time int)
     (hits-per-url :reader log-stats-hits-per-url
		   :initarg :hits-per-url
		   :initform nil)       ; '(url int)
     (hits-per-user-agent :reader log-stats-hits-per-user-agent
                          :initarg :hits-per-user-agent
                          :initform nil) ; '(user-agent int)
     (hits-per-referer :reader log-stats-hits-per-referer
		       :initarg :hits-per-referer
		       :initform nil)   ; '(url int)
     (new-referers :reader log-stats-new-referers
		   :initarg :new-referers
		   :initform nil)       ; '(url int)
     (sessions-per-user  :reader log-stats-sessions-per-user
			 :initarg :sessions-per-user
			 :initform nil) ; '(user int)
     (sessions-per-host :reader log-stats-sessions-per-host
			:initarg :sessions-per-host
			:initform nil)  ; '(host int)
     (sessions-per-length :reader log-stats-sessions-per-length
			  :initarg :sessions-per-length
			  :initform nil) ; '(int int)
     )))

(define-persistent-class web-server-stats (store-object log-stats)
  ((date :read :index-type unique-index
               :index-reader web-server-stats-with-date)))

(defmethod bknr.datastore::store-object-persistent-slots :around ((object web-server-stats))
  (append '(date sessions hits hits-per-hour hits-per-url hits-per-referer
	    sessions-per-user sessions-per-host sessions-per-length)
	  (call-next-method)))

(defun all-web-server-stats ()
  (store-objects-with-class 'web-server-stats))

(defun get-daily-stats (date month year)
  (web-server-stats-with-date (day-interval date month year)))
	 
(defun analyze-log-events (events &key remove-referer-hosts remove-urls)
  "Analyze the events given as argument, presumably a day's log"
  (multiple-value-bind (hit-sum hour-hash url-hash referer-hash user-agent-hash)
      (count-multiple events
		      #'(lambda (e) (get-hourtime (event-time e)))
		      #'(lambda (e) (let ((url (web-server-log-event-url e)))
				      (unless (member url remove-urls :test #'equal)
					url)))
		      #'(lambda (e) (let ((referer (web-server-log-event-referer e)))
				      #+(or)
				      (format t "referer: ~A uri-host: ~A, vhosts: ~A, member: ~A~%"
					      referer
					      (uri-host (parse-uri referer))
					      remove-referer-hosts (member (ignore-errors (uri-host (parse-uri referer)))
									   remove-referer-hosts :test #'equal))
				      (unless (member (ignore-errors (uri-host (parse-uri referer)))
						      remove-referer-hosts :test #'equal)
					referer)))
		      #'(lambda (e) (web-server-log-event-user-agent e)))
    (values hit-sum
	    (sort (remove nil (hash-to-list hour-hash) :key #'car)
		  #'< :key #'car)
	    (subseq* (remove nil (hash-to-list url-hash) :key #'car) 0 20)
	    (subseq* (remove nil (hash-to-list referer-hash) :key #'car) 0 20)
	    (subseq* (remove nil (hash-to-list user-agent-hash) :key #'car) 0 20))))

(defun analyze-sessions (events)
  (multiple-value-bind (session-sum user-hash host-hash)
      (count-multiple events
		      #'(lambda (event) (ignore-errors (user-login (web-visitor-event-user event))))
		      #'(lambda (event) (ignore-errors (host-name (web-visitor-event-host event)))))
    (values session-sum
	    (subseq* (hash-to-list user-hash) 0 20)
	    (subseq* (hash-to-list host-hash) 0 20))))

(defun make-daily-stats (day month year &key remove-referer-hosts remove-urls delete-events)
  (multiple-value-bind (from to)
      (day-interval day month year)
    (cond
      ((> to (get-universal-time)) nil)
      ((get-daily-stats day month year)
       (get-daily-stats day month year))
      (t (let ((log-events (find-events :from from :to to :class 'bknr.web::web-server-log-event :include-subclasses nil))
	       (enter-events (find-events :from from :to to :class 'bknr.web::web-visitor-event
                                                            :include-subclasses nil))
	       (leave-events (find-events :from from :to to :class 'bknr.web::web-visitor-left-event)))
	   
	   (multiple-value-bind (hits hits-per-hour hits-per-url hits-per-referer hits-per-user-agent)
	       (analyze-log-events log-events
				   :remove-referer-hosts remove-referer-hosts
				   :remove-urls remove-urls)
	     (multiple-value-bind (sessions sessions-per-user sessions-per-host)
		 (analyze-sessions enter-events)
	       
	       (let ((session-id-hash (make-hash-table :test #'equal))
		     (session-length-hash (make-hash-table :test #'eql)))
		 (dolist (event enter-events)
		   (when (web-visitor-event-session-id event)
		     (setf (gethash (web-visitor-event-session-id event) session-id-hash) event)))
		 (dolist (event leave-events)
		   (when (web-visitor-event-session-id event)
		     (let* ((start-event (gethash (web-visitor-event-session-id event) session-id-hash))
			    (start-time (when start-event (event-time start-event)))
			    (length (when start-time (round (- (event-time event) start-time) 5))))
		       (when length
			 (incf-hash (round (/ length 60)) session-length-hash)))))
		 (let ((ret (make-instance 'web-server-stats
                                           :sessions-per-length (hash-to-list session-length-hash)
                                           :date from
                                           :sessions sessions :hits hits :hits-per-hour hits-per-hour
                                           :hits-per-url hits-per-url :sessions-per-user sessions-per-user
                                           :hits-per-referer hits-per-referer
                                           :hits-per-user-agent hits-per-user-agent
                                           :sessions-per-host sessions-per-host)))
		   (when delete-events
		     (let ((events (append log-events enter-events leave-events)))
		       (when events
			 (apply #'delete-objects events))))
		   ret)))))))))

(defun merge-stats (stats)
  (flet ((merge-stat-lists (accessor &key count)
	   (let ((hash (make-hash-table :test #'equal)))
	     (dolist (list (mapcar accessor stats))
	       (dolist (elt list)
		 (incf-hash (car elt) hash (cdr elt))))
	     (if count
                 (subseq* (hash-to-list hash) 0 count)
                 (hash-to-list hash)))))
    (make-instance 'log-stats
		   :sessions (apply #'+ (mapcar #'log-stats-sessions stats))
		   :hits (apply #'+ (mapcar #'log-stats-hits stats))
		   :sessions-per-length (merge-stat-lists #'log-stats-sessions-per-length
							  :count 20)
		   :hits-per-hour (sort (merge-stat-lists #'log-stats-hits-per-hour)
					#'< :key #'car)
		   :hits-per-url (merge-stat-lists #'log-stats-hits-per-url
						   :count 20)
		   :sessions-per-user (merge-stat-lists #'log-stats-sessions-per-user
							:count 20)
		   :hits-per-referer (merge-stat-lists #'log-stats-hits-per-referer)
		   :hits-per-user-agent (merge-stat-lists #'log-stats-hits-per-user-agent)
		   :sessions-per-host (merge-stat-lists #'log-stats-sessions-per-host
							:count 20))))

(defun make-monthly-stats (month year)
  (let ((stats (loop for date from 1 to (month-num-days month year)
                  for stat = (get-daily-stats date month year)
                  collect stat)))
    (values (merge-stats (remove nil stats))
	    stats)))

(defun make-yearly-stats (year)
  (let ((stats (loop for month from 1 to 12
                  for stat = (make-monthly-stats month year)
                  collect stat)))
    (values (merge-stats (remove nil stats)) stats)))

(defun make-yesterdays-stats (&key remove-referer-hosts remove-urls delete-events)
  (let* ((time (get-universal-time))
	 (yesterday (- time (* 24 60 60))))
    (multiple-value-bind (second minute hour date month year)
	(decode-universal-time yesterday)
      (declare (ignore second minute hour))
      (make-daily-stats date month year :remove-referer-hosts remove-referer-hosts
			:remove-urls remove-urls :delete-events delete-events))))

