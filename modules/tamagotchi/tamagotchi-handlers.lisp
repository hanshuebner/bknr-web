(in-package :bknr-user)

(enable-interpol-syntax)

(defmethod object-url ((tamagotchi tamagotchi))
  (format nil "/image/~a"
	  (tamagotchi-name tamagotchi)))

(defclass tamagotchi-handler (edit-object-handler)
  ())

(defmethod authorized-p ((handler tamagotchi-handler))
  (if (string-equal (request-method) "GET")
      t
      (let ((tamagotchi (object-handler-get-object handler)))
	(cond ((null tamagotchi) t)
	      ((null (tamagotchi-owner tamagotchi)) t)
	      ((equal (bknr-session-user) (tamagotchi-owner tamagotchi)) t)
	      (t nil)))))

(defmethod object-handler-get-object ((handler tamagotchi-handler))
  (find-store-object (parse-url) :class 'tamagotchi :query-function #'tamagotchi-with-name))

(defmethod handle-object-form ((handler tamagotchi-handler)
			       action (tamagotchi (eql nil)))
  (with-bknr-page (:title "all tamagotchis")
    (:ul (dolist (tamagotchi (all-tamagotchis))
	   (html (:li ((:a :href (format nil "/tamagotchi/~A"
					 (tamagotchi-name tamagotchi)))
		       (:princ-safe (tamagotchi-name tamagotchi)))))))))

(defmethod handle-object-form ((handler tamagotchi-handler)
			       action tamagotchi)
  (let ((name (tamagotchi-name tamagotchi)))
    (with-bknr-page (:title #?"tamagotchi: ${name}")
      (tamagotchi :id (store-object-id tamagotchi)))))

(defmacro deftamagotchi-action (action transaction)
  `(defmethod handle-object-form ((handler tamagotchi-handler)
				 (action (eql ,action))
				 tamagotchi)
    (when (and tamagotchi)
;	       (>= (- (get-universal-time)
;		      (tamagotchi-last-action-time tamagotchi)) 10))
      (,transaction tamagotchi :time (get-universal-time)))
    (redirect (object-url tamagotchi))))

(deftamagotchi-action :feed tamagotchi-feed)
(deftamagotchi-action :play tamagotchi-play)
(deftamagotchi-action :joint tamagotchi-joint)
(deftamagotchi-action :ecstasy tamagotchi-ecstasy)
(deftamagotchi-action :coffee tamagotchi-coffee)
(deftamagotchi-action :clean tamagotchi-clean)
(deftamagotchi-action :hurt tamagotchi-hurt)
(deftamagotchi-action :beer tamagotchi-beer)

