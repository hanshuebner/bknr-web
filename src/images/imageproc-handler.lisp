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

(defun imageproc% (working-image operations)
  (let* ((was-true-color-p (true-color-p working-image)))
    (setf (save-alpha-p working-image) t)
    (dolist (operation operations)
      (destructuring-bind (operation-name &rest args) (substitute nil "" (split "," operation) :test #'equal)
        (when-let ((returned-image (apply-imageproc-operation operation-name args working-image)))
          (unless (eq working-image returned-image)
            (destroy-image working-image)
            (setf working-image returned-image)))))
    (when (and (not was-true-color-p)
               (true-color-p working-image))
      (true-color-to-palette :dither t :image working-image :colors-wanted 256))
    working-image))

(defun imageproc-gif (store-image operations)
  (let* ((input-stream (skippy:load-data-stream (blob-pathname store-image)))
         (tmp-pathname (temporary-file:with-open-temporary-file (tmp-stream)
                         (pathname tmp-stream)))
         output-stream
         #+keep-gif-resize-frames (frame-no 0)
         (frame (create-image (skippy:width input-stream) (skippy:height input-stream) t))
         (previous-frame (create-image (skippy:width input-stream) (skippy:height input-stream) t)))
    (setf (alpha-blending-p frame) t
          (alpha-blending-p previous-frame) t)
    (draw-rectangle* 0 0
                     (skippy:width input-stream) (skippy:height input-stream)
                     :filled t
                     :color (find-color 255 255 255 :alpha 0 :image frame)
                     :image frame)
    (dolist (input-frame (coerce (skippy:images input-stream) 'list))
      (skippy:output-data-stream (skippy:make-data-stream :width (skippy:width input-stream)
                                                          :height (skippy:height input-stream)
                                                          :color-table (or (skippy:color-table input-frame)
                                                                           (skippy:color-table input-stream))
                                                          :initial-images (list input-frame))
                                 tmp-pathname)
      #+keep-gif-resize-frames
      (copy-file tmp-pathname (make-pathname :name (format nil "~A-~D" (pathname-name tmp-pathname) (incf frame-no))
                                             :type "gif"
                                             :defaults tmp-pathname))
      (let ((raw-image (create-image-from-file tmp-pathname :gif)))
        (copy-image frame previous-frame 0 0 0 0 (image-width frame) (image-height frame))
        (copy-image raw-image frame
                    0 0
                    (skippy:left-position input-frame) (skippy:top-position input-frame)
                    (skippy:width input-frame) (skippy:height input-frame))
        (destroy-image raw-image))
      (let ((transformed-frame (create-image (image-width frame) (image-height frame) t)))
        (copy-image frame transformed-frame 0 0 0 0 (image-width frame) (image-height frame))
        (setf transformed-frame (imageproc% transformed-frame operations))
        (unwind-protect
             (progn
               (true-color-to-palette :dither t :image transformed-frame)
               (write-image-to-file tmp-pathname :image transformed-frame :type :gif :if-exists :supersede)
               #+keep-gif-resize-frames
               (copy-file tmp-pathname (make-pathname :name (format nil "~A-~D-processed" (pathname-name tmp-pathname) frame-no)
                                                      :type "gif"
                                                      :defaults tmp-pathname))
               (unless output-stream
                 (setf output-stream (skippy:make-data-stream :width (image-width transformed-frame)
                                                              :height (image-height transformed-frame)
                                                              :loopingp (skippy:loopingp input-stream)
                                                              :color-table (skippy:color-table input-stream))))
               (let* ((resized-frame-stream (skippy:load-data-stream tmp-pathname))
                      (resized-frame (aref (skippy:images resized-frame-stream) 0)))
                 (skippy:make-image :width (skippy:width output-stream)
                                    :height (skippy:height output-stream)
                                    :data-stream output-stream
                                    :image-data (skippy:image-data resized-frame)
                                    :color-table (or (skippy:color-table resized-frame)
                                                     (skippy:color-table resized-frame-stream))
                                    :delay-time (skippy:delay-time input-frame))))
          (destroy-image transformed-frame)))
      (ecase (skippy:disposal-method input-frame)
        ((:none :unspecified))
        (:restore-background
         (draw-rectangle* (skippy:left-position input-frame) (skippy:top-position input-frame)
                          (skippy:width input-frame) (skippy:height input-frame)
                          :filled t
                          :color (find-color 255 255 255 :alpha 0 :image frame)
                          :image frame))
        (:restore-previous
         (copy-image previous-frame frame 0 0 0 0 (image-width previous-frame) (image-height previous-frame)))))
    (destroy-image frame)
    (destroy-image previous-frame)
    (delete-file tmp-pathname)
    (with-http-response (:content-type (image-content-type :gif))
      (let ((stream (send-headers)))
        ;; do what emit-image-to-browser does
        (skippy:write-data-stream output-stream stream)
        (finish-output stream)))))

(defvar *current-image*)

(defun imageproc (store-image operations)
  (let ((*current-image* store-image))
    (if (eq :gif (image-type-keyword store-image))
        (imageproc-gif store-image operations)
        (let* ((working-image (create-image-from-file (blob-pathname store-image) (image-type-keyword store-image)))
               (processed-image (imageproc% working-image operations)))
          (unwind-protect
               (emit-image-to-browser processed-image (image-type-keyword store-image))
            (destroy-image processed-image))))))
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
  (with-default-image (input-image)
    (let ((colors (loop for (old new) on color-mappings by #'cddr
                        collect (cons (parse-color old) (parse-color new)))))
      (do-pixels (input-image)
        (let ((new-color (cdr (assoc (ldb (byte 24 0) (raw-pixel)) colors)))
              (transparency (ldb (byte 8 24) (raw-pixel))))
          (when new-color
            (setf (ldb (byte 8 24) new-color) transparency)
            (setf (raw-pixel) new-color))))))
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

(defclass imageproc-handler (cachable-handler image-handler)
  ())

(defmethod initialize-instance :after ((handler imageproc-handler) &rest initargs)
  (declare (ignore initargs))
  (setf (handler-max-age handler) (* 60 60 24 365)))

(defmethod handle-object ((page-handler imageproc-handler) (image (eql nil)))
  (error-404))

(defmethod handle-object ((page-handler imageproc-handler) image)
  (handle-if-modified-since (blob-timestamp image))
  (setf (header-out :last-modified) (rfc-1123-date (blob-timestamp image)))
  (let ((operations (cdr (decoded-handler-path page-handler))))
    (if operations
        (imageproc image operations)
        (with-http-response (:content-type (image-content-type (image-type-keyword image)))
          (setf (header-out :content-length) (blob-size image))
          (with-open-file (blob-data (blob-pathname image) :element-type '(unsigned-byte 8))
            (copy-stream blob-data (send-headers) :element-type '(unsigned-byte 8)))))))
