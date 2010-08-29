(in-package :leech)

;;; wer ein beispiel braucht soll sich bei manuel melden
;;; das passt hier nicht ganz in den rahmen rein :)

(defgeneric schedule-job (queue job))
(defgeneric execute-job (queue job))
(defgeneric job-queue-add-job (queue job))
(defgeneric job-error-handler (queue job e))
(defgeneric job-queue-print (queue))
(defgeneric job-queue-job-count (queue))
(defgeneric job-queue-idle-p (queue))
(defgeneric job-queue-destroy (queue))
(defgeneric job-queue-start (queue))
(defgeneric execute-leech-job (queue job content))

(defvar *queue* nil)
(defvar *job* nil)

(defclass job ()
  ((name :initarg :name
	 :reader job-name
	 :initform nil)
   (fun :initarg :fun
	:reader job-fun)))

(defmethod print-object ((job job) s)
  (print-unreadable-object (job s :type t)
    (format s "~A" (job-name job))))

(defclass job-queue ()
  ((proc :initarg :process
	 :accessor queue-process
	 :initform nil)
   (jobs :initarg :jobs
	 :accessor queue-jobs
	 :initform nil)
   (errors :accessor queue-errors
	   :initform nil)
   (name :initarg :name
	 :reader queue-name
	 :initform "Job queue")
   (waiting :accessor queue-waiting
	    :initform nil)
   (current :accessor queue-current
	    :initform nil)
   (lock :initarg :lock
	 :accessor queue-lock
	 :initform (acl-compat.mp:make-process-lock))))

(defmethod initialize-instance :after ((queue job-queue) &rest initargs)
  (declare (ignore initargs))
  (job-queue-start queue))

(defgeneric job-queue-run-function (queue))
(defmethod job-queue-run-function ((queue job-queue))
  (format t "~A running~%" queue)
  (do ()
      (nil)
    #+nil
    (format t "~A waiting for jobs~%" queue)
    (setf (queue-waiting queue) t)
    (acl-compat.mp:process-wait "Waiting for jobs"
				#'(lambda (queue)
				    (not (null (queue-jobs queue)))) queue)
    ;; lock list?
    (setf (queue-waiting queue) nil)
    (let ((job (acl-compat.mp:with-process-lock ((queue-lock queue))
		 (pop (queue-jobs queue)))))
      (setf (queue-current queue) job)
      #+nil(format t "~A executing job ~A~%" queue job)
      (schedule-job queue job)
      (setf (queue-current queue) nil))))

(defmethod schedule-job ((queue job-queue) (job job))
  (execute-job queue job))

(defmethod execute-job :around ((queue job-queue) (job job))
  (let ((*queue* queue)
	(*job*   job))
    (handler-case (call-next-method)
      (error (e) (job-error-handler queue job e)))))

(defmethod job-error-handler ((queue job-queue) (job job) e)
  (warn "Error ~A while executing ~A on ~A" e job queue)
  (push (list job e) (queue-errors queue))
  #+nil(error e))

(defmethod execute-job ((queue job-queue) (job job))
  (funcall (job-fun job)))

(defmethod job-queue-job-count ((queue job-queue))
  (length (queue-jobs queue)))

(defgeneric job-queue-add-job (queue job))

(defmethod job-queue-print ((queue job-queue))
  (acl-compat.mp:with-process-lock ((queue-lock queue))
    (when (queue-current queue)
      (format t "CURRENT. ~A~%" (queue-current queue)))
    (loop for i from 0
	  for (name fun) in (queue-jobs queue)
	  do (format t "~A. ~A~%" i name))))

(defmethod job-queue-idle-p ((queue job-queue))
  (acl-compat.mp:with-process-lock ((queue-lock queue))
    (and (queue-waiting queue)
	 (null (queue-jobs queue)))))

(defmethod job-queue-add-job ((queue job-queue) job)
  (acl-compat.mp:with-process-lock ((queue-lock queue))
    #+nil(format t "adding job ~s to ~A~%" name queue)
    (setf (queue-jobs queue)
	  (nconc (queue-jobs queue) (list job)))))

(defmethod job-queue-destroy ((queue job-queue))
  (acl-compat.mp:process-kill (queue-process queue))
  (setf (queue-process queue) nil
	(queue-jobs queue) nil))

(defmethod job-queue-start :before ((queue job-queue))
  (when (queue-process queue)
    (warn "Job queue is already running")))

(defmethod job-queue-start ((queue job-queue))
  (let ((process (acl-compat.mp:process-run-function
		  (queue-name queue)
		  #'(lambda ()
		      (job-queue-run-function queue)))))
    (setf (queue-process queue) process)))

(defclass leech-queue (job-queue)
  ((proxy :initarg :proxy
	  :accessor queue-proxy
	  :initform nil)
   (cookie-jar :initarg :cookie-jar
	       :accessor queue-cookie-jar
	       :initform nil)))

(defmethod job-queue-start :before ((queue leech-queue))
  (setf (queue-cookie-jar queue)
	(make-instance 'net.aserve.client:cookie-jar)))

(define-condition leech-error (error)
  ())
(defmethod leech-url ((queue leech-queue) url &rest args)
  (handler-case (apply #'net.aserve.client:do-http-request url
		       :cookies (queue-cookie-jar queue)
		       :proxy (queue-proxy queue)
		       args)
    (error (e)
      (declare (ignore e))
      (error (make-instance 'leech-error)))))

(defclass leech-job (job)
  ((url :initarg :url
	:reader job-url)))

(defmethod execute-job ((queue leech-queue) (job leech-job))
  (execute-leech-job queue job (leech-url queue (job-url job))))

(defclass leech-file-job (leech-job)
  ((file :initarg :file
	 :reader job-file)))

(defun uri-make-pathname (uri pathname)
  (let ((uri-path (parse-namestring (puri:uri-path uri))))
    (make-pathname :name (pathname-name uri-path)
		   :type (pathname-type uri-path)
		   :defaults pathname)))

(defmethod execute-leech-job ((queue leech-queue) (job leech-file-job) content)
  (ensure-directories-exist (make-pathname :directory (pathname-directory (job-file job))))
  (let ((file (if (pathname-name (job-file job))
		  (job-file job)
		  (uri-make-pathname (puri:parse-uri (job-url job)) (job-file job)))))
    (with-open-file (s file :direction :output)
      (write-sequence content s))))

(defclass child-leech-queue (leech-queue)
  ((parent-queue :initarg :parent-queue
		 :reader parent-queue)))

(defmethod job-error-handler ((queue child-leech-queue) (job job) e)
  (job-error-handler (parent-queue queue) job e))

(defmethod execute-job ((queue child-leech-queue) (job job))
  (execute-job (parent-queue queue) job))

(defclass pleech-queue (leech-queue)
  ((leech-queues :initarg :queues
		 :accessor pleech-queues
		 :initform nil)
   (proxies :initarg :proxies
	    :accessor pleech-proxies
	    :initform nil)))

(defmethod schedule-job ((queue pleech-queue) job)
  (do ()
      (nil)
    (acl-compat.mp:process-wait "Waiting for empty leech queue"
				#'(lambda (queue)
				    (some #'job-queue-idle-p (pleech-queues queue))) queue)
    (let ((q (find-if #'job-queue-idle-p (pleech-queues queue))))
      #+nil(format t "adding job ~A to child queue ~A~%" job q)
      (when q
	(job-queue-add-job q job)
	(return-from schedule-job)))))

(defun load-proxies (file)
  (with-open-file (s file :direction :input)
    (loop for proxy = (read-line s nil 'eof)
	  until (eql proxy 'eof)
	  collect (subseq proxy 7 (1- (length proxy))))))

(defmethod job-queue-start :before ((queue pleech-queue))
  (when (pleech-queues queue)
    (error "~A is still running, destroy first" queue)))

(defmethod job-queue-start :before ((queue pleech-queue))
  (setf (pleech-queues queue)
	(if (null (pleech-proxies queue))
	    (list (make-instance 'child-leech-queue
				 :cookie-jar (queue-cookie-jar queue)
				 :parent-queue queue))
	    (mapcar #'(lambda (proxy)
			(make-instance 'child-leech-queue
				       :parent-queue queue
				       :proxy proxy
				       :cookie-jar (queue-cookie-jar queue)))
		    (pleech-proxies queue)))))

(defmethod job-queue-destroy :before ((queue pleech-queue))
  (dolist (queue (pleech-queues queue))
    (job-queue-destroy queue))
  (setf (pleech-queues queue) nil))

(defmethod job-queue-print ((queue pleech-queue))
  (loop for i from 0
	for queue in (pleech-queues queue)
	do (format t "~A. QUEUE ~A~%" i queue)
	(job-queue-print queue)))

(defmethod job-queue-idle-p ((queue pleech-queue))
  (reduce #'(lambda (x1 x2) (and x1 x2))
	  (mapcar #'job-queue-idle-p (pleech-queues queue)) :initial-value t))

(defmethod job-error-handler ((queue pleech-queue) (job job) e)
  (warn "Rescheduling failed job ~A" job)
  (job-queue-add-job queue job))
