(in-package :bknr.images)

(defparameter *session-image-point-size* 20)
(defparameter *session-image-font* "/home/hans/DIN/DINBd___.ttf")

(defclass session-image-handler (page-handler)
  ())

(defmethod handle ((handler session-image-handler))
  (let ((random-string (or (session-value :random-string)
			   (format nil "~(~36,3,'0R~)" (+ 1296 (random (- 46656 1296)))))))
    (setf (session-value :random-string) random-string)
    (with-image* ((* 5 *session-image-point-size*) (* 2 *session-image-point-size*))
      (setf (transparent-color) (allocate-color 255 255 255))
      (loop with x-min = (* 5 *session-image-point-size*)
	    with x-max = 0
	    with x-pos = *session-image-point-size*
	    with y-min = (* 2 *session-image-point-size*)
	    with y-max = 0
	    for char across random-string
	    do (destructuring-bind
		     (x1 y1 x2 y2 x3 y3 x4 y4)
		   (coerce (draw-freetype-string x-pos (round (+ *session-image-point-size* (/ *session-image-point-size* 2)))
						 (make-string 1 :initial-element char)
						 :font-name *session-image-font*
						 :angle (- (/ pi 4 4) (random (/ pi 2 4)))
						 :point-size *session-image-point-size*
						 :color (find-color 0 0 0 :resolve t)) 'list)
		 (setf x-min (min x-min x1 x2 x3 x4))
		 (setf x-max (max x-max x1 x2 x3 x4))
		 (setf y-min (min y-min y1 y2 y3 y4))
		 (setf y-max (max y-max y1 y2 y3 y4))
		 (setf x-pos (1+ x-max)))
	    finally (with-image (result-image (- x-max x-min) (- y-max y-min))
		      (setf (transparent-color result-image) (allocate-color 255 255 255 :image result-image))
		      (copy-image *default-image* result-image x-min y-min 0 0 (image-width result-image) (image-height result-image))
		      (emit-image-to-browser result-image :png))))))
