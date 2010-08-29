(in-package :bknr-user)

(defstruct (player-state (:conc-name ps-)) 
  mp3 length remain)

(defstruct (player-command (:conc-name pc-))
  command args)

(defclass player (bknr-actor)
  ((state :accessor player-state :initform (make-player-state :mp3 nil
							      :length 0
							      :remain 0))
   (queue :accessor player-queue :initform nil)
   (lock :accessor player-lock :initform (mp-make-lock))))

(defgeneric player-play (player mp3))
(defgeneric player-stop (player))
(defgeneric player-song-terminated (player))
(defgeneric state (player)
  (:documentation "returns state struct"))
(defgeneric send-command (player command))
(defgeneric queue-command (player command))
(defgeneric dequeue-command (player))

(defmethod queue-command ((player player) command)
  (with-slots (queue lock) player
    (mp-with-lock-held (lock)
      (setf queue (append queue (list command))))))

(defmethod dequeue-command ((player player))
  (with-slots (queue lock) player
    (mp-with-lock-held (lock)
      (pop queue))))

(defmethod player-play ((player player) mp3)
  (queue-command player (make-player-command :command :play :args (list mp3))))

(defmethod player-stop ((player player))
  (queue-command player (make-player-command :command :stop :args nil)))

(define-persistent-class player-play-event (event)
  ((mp3 :read :initform nil)))

(define-persistent-class player-stop-event (event)
  ())

(defclass mpg123-player (player)
  ((stream :accessor mpg123-player-stream :initform nil)
   (process :accessor mpg123-player-process :initform nil)))

(defmethod start-mpg123 ((player mpg123-player))
  (let ((proc (run-program "mpg123" '("-R")
			   :wait nil
			   :input :stream
			   :output :stream
			   :error :output
			   :status-hook #'(lambda (proc)
					    (declare (ignore proc))
					    (mpg123-status-changed player)))))
    (when proc
      (with-slots (stream process state) player
	(setf stream (make-two-way-stream (process-output proc)
					  (process-input proc))
	      process proc)))))

(defmethod mpg123-status-changed ((player mpg123-player))
  (format t "status changed~%")
  (let ((proc (mpg123-player-process player)))
    (case (process-status proc)
      (:running)
      (:stopped (format t "mpg123 stopped~%"))
      (:signaled (format t "mpg123 stopped~%"))
      (t (format t "mpg123 has status: ~a~%" (process-status proc))))))

(defmethod send-command ((player mpg123-player) command)
  (with-slots (process stream) player
    (when (null process)
      (error "mpg123 is not running"))
    (case (pc-command command)
      (:play (let ((mp3 (first (pc-args command))))
	       (setf (ps-mp3 (player-state player)) mp3)
	       (format stream "LOAD ~A~%"
		       (blob-pathname (first (pc-args command))))))
      (:stop (write-string "STOP" stream) (terpri stream))
      (t (error "unknown command")))
    (force-output stream)))

(defmethod actor-stop :before ((player mpg123-player))
  (when (mpg123-player-process player)
    (process-kill (mpg123-player-process player) 9)))

(defmethod mpg123-player-parse-status ((player mpg123-player) msg)
  #+nil(format t "msg: ~a~%" msg)
  (unless (eql (char msg 0) #\@)
    (warn "Unknown message format")
    (format t "msg: ~a~%" msg)
    (return-from mpg123-player-parse-status))
  (case (char msg 1)
    (#\E (error (format nil "Error from mpg123: ~a" (subseq msg 3))))
    (#\P (when (eql (char msg 3) #\0)
	   (make-event 'player-stop-event)))
    (#\F (let ((args (mapcar #'(lambda (str) (parse-integer str :junk-allowed t))
			     (split "\\s+" (subseq msg 3)))))
	   (setf (ps-remain (player-state player)) (fourth args)
		 (ps-length (player-state player)) (+ (third args)
						      (fourth args)))))
    (#\I (make-event 'player-play-event :mp3 (ps-mp3 (player-state player))))))

#+(or) ; process-sleep missing?
(defmethod run-function ((player mpg123-player))
  (format t "; mpg123 media player actor started~%")
  (loop (when (mpg123-player-process player)
	  (process-kill (mpg123-player-process player) 9))
	(start-mpg123 player)
	(with-slots (process lock queue stream) player
	  (loop for char = (read-char-no-hang stream nil 'eof)
		do (cond ((eql char 'eof)
			  (return))
			 (queue
			  (send-command player (dequeue-command player)))
			 ((null char)
			  (process-sleep 1))
			 (t (unread-char char stream)
			    (let ((msg (read-line stream nil 'eof)))
			      (mpg123-player-parse-status player msg))))))))
	  
(defparameter *main-player* (make-instance 'mpg123-player :name "Mpg123 player"))
