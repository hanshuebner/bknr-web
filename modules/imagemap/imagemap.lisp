(in-package :bknr-user)

(enable-interpol-syntax)

(define-persistent-class imagemap ()
  ((name :read
	 :index-type string-unique-index
	 :index-reader imagemap-with-name)
   
   (image :update)
   (clickable-areas :update :initform nil)))

(defun find-imagemap (name &key create)
  (or (imagemap-with-name name)
      (and create
	   (let ((image (find-image name)))
	     (unless image
	       (error "can't find image named ~a to create image map of same name" name))
	     (make-instance 'imagemap :name name :image image)))))

(defmethod imagemap-html-areadefs ((imagemap imagemap) &key (name (imagemap-name imagemap)))
  (html ((:map :name name)
	 (loop for area in (imagemap-clickable-areas imagemap)
            do (html ((:area :shape "POLY"
                             :coords (format nil "~{~a~#[~:;,~]~}" (flatten (imagemap-area-polygon area)))
                             :alt (imagemap-area-url area)
                             :href (imagemap-area-url area))))))))

(define-persistent-class imagemap-area ()
  ((url :update)
   (polygon :update)))

(deftransaction add-clickable-area (imagemap &key url polygon)
  (setf (imagemap-clickable-areas imagemap) (cons (make-instance 'imagemap-area :url url :polygon polygon)
						  (imagemap-clickable-areas imagemap))))

(deftransaction delete-clickable-area (imagemap area)
  (setf (imagemap-clickable-areas imagemap) (remove area (imagemap-clickable-areas imagemap)))
  (delete-store-object area))

(defun point-in-polygon-p (point polygon)
  (let (result
	(py (second point)))
    (loop with (pjx pjy) = (last polygon 2)
       for (pix piy) on polygon by #'cddr
       when (and (or (and (<= piy py) (< py pjy))
                     (and (<= pjy py) (< py piy)))
                 (< (first point)
                    (+ (/ (* (- pjx pix) (- py piy))
                          (- pjy piy))
                       pix)))
       do (setf result (not result))
       do (setf pjx pix
                pjy piy))
    result))

(defmethod imagemap-clickable-area-at ((imagemap imagemap) point)
  "Return imagemap-area at given point or nil if the given point is not within a clickable
area of the image map"

  (dolist (area (imagemap-clickable-areas imagemap))
    (when (point-in-polygon-p point (imagemap-area-polygon area))
      (return area))))
