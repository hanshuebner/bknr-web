(in-package :bknr-user)

(defclass dj ()
  ((player :initarg :player :reader dj-player :initform nil)))

#+(or)
(defmethod initialize-instance :after ((dj dj) &rest initargs)
  (declare (ignore initargs))
  (register-event-handler 'player-stop-event ; xxx needs to use the
					     ; new event handling
					     ; framework (i.e. derive
					     ; from event-handler and
					     ; provide a handle-event
					     ; method for the classes
					     ; it wants to handle
			  #'(lambda (e)
			      (dj-player-stopped dj e)))
  (actor-start (dj-player dj)))

(defmethod dj-player-stopped ((dj dj) e)
  (dj-play dj))

(defmethod dj-play ((dj dj))
  (let ((track (dj-next-track dj)))
    (when track
      (unless (actor-running-p (dj-player dj))
	(actor-start (dj-player dj)))
      (player-play (dj-player dj) track))))

(defmethod dj-stop ((dj dj))
  (actor-stop (dj-player dj)))

(defmethod dj-next-track ((dj dj))
  nil)

(defclass tama-dj (dj)
  ((tamagotchi :initarg :tamagotchi :reader dj-tamagotchi :initform nil)
   (style :initarg :style :reader dj-style :initform nil)
   (store :initarg :store :reader dj-store)
   (played-tracks :initform nil :accessor dj-played-tracks)))

(defmethod dj-next-track ((dj tama-dj))
  (let ((tracks (randomize-list
		 (set-difference
		  (get-genres-intersection-tracks
		   (list (dj-style dj)
			 (tamagotchi-status (dj-tamagotchi dj))))
		  (dj-played-tracks dj)))))
    (when (null tracks)
      (setf tracks (randomize-list (set-difference (all-tracks)
						   (dj-played-tracks dj)))))
    (let ((track (pop tracks)))
      (push track (dj-played-tracks dj))
      track)))

(defclass random-dj (dj)
  ((tracks :initform nil :accessor random-dj-tracks
	   :documentation "Liste der aktuell zu spielenden Tracks.")
   (current-track :initform nil :accessor random-dj-current-track
		  :documentation "Aktuell laufender track")
   (min-tracks-length :initform 10 :accessor random-dj-min-tracks-length)))

(defmethod select-next-track ((dj random-dj))
  (random-elt (all-tracks)))

(defmethod random-dj-add-track-to-list ((dj random-dj))
  (with-slots (tracks current-track) dj
    (setf tracks (append tracks (list (select-next-track dj))))))

(defmethod dj-next-track ((dj random-dj))
  (with-slots (tracks current-track min-tracks-length) dj
    (loop while (< (length tracks)
		   min-tracks-length)
	  do (random-dj-add-track-to-list dj))
    (setf current-track
	  (if current-track
	      (second (member current-track tracks))
	      (first tracks)))))

(defmethod make-track-info-html ((track track))
  (with-output-to-string (*standard-output*)
    (mapc #'(lambda (slot-name)
	      (html-stream *standard-output*
			   ((:div :class (format nil "track_~(~A~)" (symbol-name slot-name)))
			    (:princ-safe (slot-value track slot-name)))))
	  '(name artist genre))))

(defmethod random-dj-as-javascript ((dj random-dj))
  (with-output-to-string (*standard-output*)
    (with-slots (tracks current-track) dj
      (format t "var mytracklist = new Array();~%")
      (loop for index from 0
	    for track in tracks
	    do (with-slots (artist name) track
		 (format t "mytracklist[~D] = new Track(~S, ~S, ~S);~%"
			 index artist name (make-track-info-html track))))
      (format t "var currenttrack = ~D;~%" (position current-track tracks)))))