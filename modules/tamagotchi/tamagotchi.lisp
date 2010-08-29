(in-package :bknr-user)

;; <meta http-equiv="refresh" content="10; URL=foo">

(define-persistent-class tamagotchi ()
  ((name :read
	 :index-type string-unique-index
	 :index-reader tamagotchi-with-name
	 :index-keys all-tamagotchis)
   (owner :update :initform nil
	  :index-type hash-index)

   (tama-type :read :initform :tama-1)
   
   ;; counters are from 0 to 10
   (stress :update :initform 5)
   (happiness :update :initform 5)

   ;; when > 3, leads to a loss of an additional minute each minute
   ;; when > 3, leads to an additional stress point each minute
   ;; hunger is incremented each 10 minutes
   (hunger :update :initform 0)
   (tired :update :initform 0)
   (sleeping :update :initform nil)

   ;; when > 3, leads to a loss of an additional ttl minute each minute
   ;; when > 3, leads to an additional stress point each minute
   ;; poop is incremented at each meal
   (poop :update :initform 0)

   (last-action-time :update :initform (get-universal-time))))

(defmethod print-object ((tamagotchi tamagotchi) stream)
  (format stream "#<~a ~a ID: ~A>"
	  (class-name (class-of tamagotchi))
	  (tamagotchi-name tamagotchi)
	  (store-object-id tamagotchi))
  tamagotchi)

(deftransaction tamagotchi-set-user (tamagotchi user)
  (setf (tamagotchi-owner tamagotchi) user))

(deftransaction tamagotchi-feed (tamagotchi &key (time (get-universal-time)))
  (with-slots (hunger poop last-action-time) tamagotchi
    (setf last-action-time time)
    (setf hunger 0)
    (unless (>= poop 10)
      (incf poop))))


(deftransaction tamagotchi-play (tamagotchi &key (time (get-universal-time)))
  (with-slots (happiness stress last-action-time) tamagotchi
    (setf last-action-time time)
    (unless (<= stress 0)
      (decf stress))
    (unless (>= happiness 10)
      (incf happiness))))

(deftransaction tamagotchi-joint (tamagotchi &key (time (get-universal-time)))
  (with-slots (stress tired last-action-time) tamagotchi
    (setf last-action-time time)
    (unless (>= tired 10)
      (incf tired))
    (unless (<= stress 0)
      (decf stress))))

(deftransaction tamagotchi-ecstasy (tamagotchi &key (time (get-universal-time)))
  (with-slots (stress happiness last-action-time) tamagotchi
    (setf last-action-time time)
    (unless (>= stress 10)
      (incf stress))
    (unless (>= happiness 10)
      (incf happiness))))

(deftransaction tamagotchi-coffee (tamagotchi &key (time (get-universal-time)))
  (with-slots (stress tired last-action-time) tamagotchi
    (setf last-action-time time)
    (unless (>= stress 10)
      (incf stress))
    (unless (<= tired 0)
      (decf tired))))

(deftransaction tamagotchi-clean (tamagotchi &key (time (get-universal-time)))
  (with-slots (poop last-action-time) tamagotchi
    (setf last-action-time time)
    (setf poop 0)))

(deftransaction tamagotchi-hurt (tamagotchi &key (time (get-universal-time)))
  (with-slots (happiness last-action-time sleeping) tamagotchi
    (when sleeping
      (setf sleeping nil))
    (setf last-action-time time)
    (unless (<= happiness 0)
      (decf happiness))))

(deftransaction tamagotchi-beer (tamagotchi &key (time (get-universal-time)))
  (with-slots (happiness poop tired hunger last-action-time) tamagotchi
    (setf last-action-time time)
    (unless (<= hunger 0)
      (decf hunger))
    (unless (>= happiness 10)
      (incf happiness))
    (unless (>= tired 10)
      (incf tired))
    (unless (>= poop 10)
      (incf poop))))

(defmethod tamagotchi-time-tick ((tamagotchi tamagotchi))
  (let ((stress (tamagotchi-stress tamagotchi))
	(happiness (tamagotchi-happiness tamagotchi))
	(hunger (tamagotchi-hunger tamagotchi))
	(poop (tamagotchi-poop tamagotchi))
	(tired (tamagotchi-tired tamagotchi))
	(sleeping (tamagotchi-sleeping tamagotchi)))
    (if sleeping
	(progn
	  (unless (<= tired 0)
	    (decf tired))
	  (unless (<= stress 0)
	    (decf stress))
	  (when (<= tired 5)
	    (setf sleeping nil)))

	(progn
	  (unless (>= hunger 10)
	    (incf hunger))
	  
	  (when (> hunger 3)
	    (unless (<= happiness 0)
	      (decf happiness)))
	  (when (> poop 3)
	    (unless (>= stress 10)
	      (incf stress)))

	  (unless (or (>= tired 10) sleeping)
	    (when (= 1 (random 5))
	      (incf tired)))
	  
	  (when (>= tired 10)
	    (setf sleeping t))))

    (change-slot-values tamagotchi 'stress stress
			'happiness happiness
			'hunger hunger
			'poop poop
			'tired tired
			'sleeping sleeping)))

(defmethod tamagotchi-status ((tamagotchi tamagotchi))
  (cond ((tamagotchi-sleeping tamagotchi) :sleep)
	((>= (tamagotchi-poop tamagotchi) 2) :poop)
	((>= (tamagotchi-happiness tamagotchi) 7) :happy)
	((>= (tamagotchi-stress tamagotchi) 7) :unhappy)
	(t :normal)))


(defclass tamagotchi-actor (bknr-datastore-actor)
  ())

(defmethod run-function ((actor tamagotchi-actor))
  (do () (nil)
    (sleep 60)
    (dolist (tamagotchi (all-tamagotchis))
      (ignore-errors (tamagotchi-time-tick tamagotchi)))))
