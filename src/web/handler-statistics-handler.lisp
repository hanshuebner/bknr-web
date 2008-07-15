(in-package :bknr.web)

(defclass handler-statistics-handler (page-handler)
 ())

(defun format-elapsed (internal-time-units)
  (format nil "~8,1F" (* 1000 (/ internal-time-units internal-time-units-per-second))))

(defmethod handle ((handler handler-statistics-handler))
  (with-bknr-page (:title "BKNR handler statistics")
    (:div "All times reported in milliseconds")
    (:table
     (:thead
      (:tr (:th "Prefix") (:th "Type") (:th "Pages") (:th "Average") (:th "Max") (:th "Min")))
     (:tbody
      (dolist (handler (website-handlers *website*))
        (let ((statistics (page-handler-statistics handler)))
          (when (and (hs-count statistics)
                     (plusp (hs-count statistics)))
            (html 
             (:tr (:td (:princ (page-handler-prefix handler)))
                  (:td (:princ (class-name (class-of handler))))
                  ((:td :align "right") (:princ (hs-count statistics)))
                  ((:td :align "right") (:princ (format-elapsed (hs-average statistics))))
                  ((:td :align "right") (let* ((slowest-array (hs-slowest statistics))
                              (slowest-entry (aref slowest-array (1- (array-dimension slowest-array 0)))))
                         (when slowest-entry
                           (html
                            (:princ (format-elapsed (car slowest-entry)))))))
                  ((:td :align "right") (let* ((fastest-array (hs-fastest statistics))
                              (fastest-entry (aref fastest-array (1- (array-dimension fastest-array 0)))))
                         (when fastest-entry
                           (html
                            (:princ (format-elapsed (car fastest-entry))))))))))))))))