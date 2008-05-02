(in-package :bknr.images)

(enable-interpol-syntax)

(defvar *imageproc-operations* (make-hash-table))

(defun register-imageproc-operation (name function)
  (setf (gethash (make-keyword-from-string (symbol-name name)) *imageproc-operations*) function))

(defmacro define-imageproc-handler (name (&rest arguments) &body body)
  `(prog1
    (eval-when (:compile-toplevel :execute)
      (defun ,name (,@arguments) ,@body))
    (eval-when (:compile-toplevel :load-toplevel :execute)
      (register-imageproc-operation ',name (fdefinition ',name)))))

(defun apply-imageproc-operation (operation-name args image)
  (apply (or (gethash (make-keyword-from-string operation-name) *imageproc-operations*)
             (error "invalid imageproc operation name ~A" operation-name))
         image args))

(defun imageproc (image operations)
  (with-store-image (input-image image)
    (setf (save-alpha-p :image input-image) t)
    (let ((working-image input-image))
      (dolist (operation operations)
        (destructuring-bind (operation-name &rest args) (substitute nil "" (split "," operation) :test #'equal)
          (let ((returned-image (apply-imageproc-operation operation-name args working-image)))
            (unless (not returned-image)
              (unless (or (eq working-image returned-image)
                          (eq working-image input-image))
                (destroy-image working-image))
              (setf working-image returned-image)))))
      (when (and (true-color-p working-image)
                 (not (true-color-p input-image)))
        (true-color-to-palette :dither t :image working-image :colors-wanted 256))
      (emit-image-to-browser working-image (image-type-keyword image))
      (unless (eq working-image input-image)
        (destroy-image working-image)))))

#+(or)
(unless (member type '(:jpg :jpeg))
  (when (true-color-p input-image)
    (true-color-to-palette :dither t :image input-image
                           :colors-wanted 256)))

(defparameter *cell-border-width* 5)

(define-imageproc-handler cell (input-image &optional (bgcolor "ffffff") (cell-width "50") (cell-height "50") (border-width "0"))
  (setq cell-width (parse-integer cell-width))
  (setq cell-height (parse-integer cell-height))
  (setf bgcolor (if (and (stringp bgcolor) (not (zerop (length bgcolor)))) bgcolor nil))
  (setq border-width (if border-width (parse-integer border-width) *cell-border-width*))
  (let* ((width (image-width input-image))
         (height (image-height input-image))
         (ratio (max (/ width (- cell-width (* border-width 2)))
                     (/ height (- cell-height (* border-width 2)))))
         (thumbnail-width (min width (round (/ width ratio))))
         (thumbnail-height (min height (round (/ height ratio))))
         (x-offset (round (/ (- cell-width thumbnail-width) 2)))
         (y-offset (round (/ (- cell-height thumbnail-height) 2)))
         (cell (create-image cell-width cell-height t)))
    (with-default-image (cell)
      (let ((color (if bgcolor
                       (parse-color bgcolor)
                       (allocate-color 255 255 255))))
        (fill-image 0 0 :color color)
        (copy-image input-image cell
                    0 0
                    x-offset
                    y-offset
                    width height
                    :resize t :resample t
                    :dest-width thumbnail-width :dest-height thumbnail-height)
        (unless bgcolor
          (setf (transparent-color) color)
          (let ((cr (ldb (byte 8 16) color))
                (cg (ldb (byte 8 8) color))
                (cb (ldb (byte 8 0) color)))
            (flet ((color-distance (c)
                     (+ (abs (- (ldb (byte 8 16) c) cr))
                        (abs (- (ldb (byte 8 8) c) cg))
                        (abs (- (ldb (byte 8 0) c) cb)))))
              (do-pixels ()
                (when (< (color-distance (raw-pixel)) 6)
                  (setf (raw-pixel) color))))))))
    cell))
  
(define-imageproc-handler thumbnail (input-image &optional bgcolor max-width max-height)
  (setf bgcolor (if (and (stringp bgcolor) (not (zerop (length bgcolor)))) bgcolor "ffffff"))
  (setf max-width (if max-width (parse-integer max-width) *thumbnail-max-width*))
  (setf max-height (if max-height (parse-integer max-height) *thumbnail-max-height*))
  (let ((width (image-width input-image))
        (height (image-height input-image)))
    (when (or (< max-width width)
              (< max-height height))
      (let* ((ratio (max (/ width max-width)
                         (/ height max-height)))
             (thumbnail-width (round (/ width ratio)))
             (thumbnail-height (round (/ height ratio)))
             (thumbnail (create-image thumbnail-width
                                      thumbnail-height
                                      t)))
        (with-default-image (thumbnail)
          (fill-image 0 0 :color (parse-color bgcolor))
          (copy-image input-image thumbnail
                      0 0 0 0
                      width height
                      :resize t :resample t
                      :dest-width thumbnail-width :dest-height thumbnail-height))
        thumbnail))))

(define-imageproc-handler double (input-image &optional (times "2"))
  (let* ((width (image-width input-image))
         (height (image-height input-image))
         (ratio (/ 1 (parse-integer times)))
         (double-image-width (round (/ width ratio)))
         (double-image-height (round (/ height ratio)))
         (double-image (create-image double-image-width double-image-height nil)))
    (with-default-image (double-image)
      (setf (transparent-color double-image)
            (find-color-from-image (transparent-color input-image) input-image :alpha t :resolve t))
      (fill-image 0 0 :color (transparent-color double-image))
      (copy-image input-image double-image
                  0 0 0 0 width height
                  :resize t
                  :dest-width double-image-width :dest-height double-image-height))
    double-image))

(define-imageproc-handler color (input-image &rest color-mappings)
  ;; We can't handle transparency, so we only transform pixels who
  ;; have 0 for their transparency value
  (with-default-image (input-image)
    (let ((colors (loop for (old new) on color-mappings by #'cddr
                        collect (cons (parse-color old) (parse-color new)))))
      (do-pixels (input-image)
        (let ((new-color (assoc (ldb (byte 24 0) (raw-pixel)) colors)))
          (when (and (zerop (ldb (byte 8 24) (raw-pixel)))
                     (cdr new-color))
            (setf (raw-pixel) (cdr new-color)))))))
  input-image)

(defun image-url (image &key process (prefix "/image"))
  (format nil "~a/~a~@[/~a~]" prefix (store-image-name image) process))

(defun parse-color (color-string &key (image *default-image*))
  (if (string-equal color-string "transparent")
      (transparent-color image)
      (let ((components (multiple-value-bind (match strings)
                            (scan-to-strings "^#?(..)(..)(..)?$" color-string)
                          (if match
                              (mapcar #'(lambda (string) (when string (parse-integer string :radix 16)))
                                      (coerce strings 'list))
                              (progn
                                (warn "can't parse color spec ~a" color-string)
                                '(0 0 0))))))
        (let ((color (find-color (first components) (second components) (third components)
                                 :exact t :image image)))
          (unless color
            (setf color (find-color (first components) (second components) (third components)
                                    :exact nil :resolve t :image image)))
          color))))

(defclass imageproc-handler (image-handler)
  ())

(defmethod handle-object ((page-handler imageproc-handler) (image (eql nil)))
  (error-404))

(defmethod handle-object ((page-handler imageproc-handler) image)
  (with-http-response (:content-type (image-content-type (image-type-keyword image)))
    (handle-if-modified-since (blob-timestamp image))
    (setf (header-out "Last-Modified") (rfc-1123-date (blob-timestamp image)))
    (imageproc image (cdr (decoded-handler-path page-handler)))))
    
