(in-package :bknr.site-menu)

(defclass menu ()
  ((name :initarg :name
	 :attribute "name"
	 :reader menu-name)
   (items :initarg items
	  :element "item"
          :containment :+
	  :reader menu-items))
  (:metaclass xml-class)
  (:element "menu"))

(defclass item ()
  ((url :initarg :url
	:attribute "url"
	:reader item-url)
   (title :initarg :title
	  :attribute "title"
	  :reader item-title)
   (inactive-image :initarg :inactive-image
		   :attribute "inactive-image"
		   :reader item-inactive-image)
   (active-image :initarg :active-image
		 :attribute "active-image"
		 :reader item-active-image)
   (hover-image :initarg :hover-image
		:attribute "hover-image"
		:reader item-hover-image))
  (:default-initargs :inactive-image nil :active-image nil :hover-image nil)
  (:metaclass xml-class)
  (:element "item"))

(defparameter *menu-def-classes* (mapcar #'find-class '(menu item)))

(defun print-menu (menu)
  (format t "MENU: ~A ITEMS:~{ ~A~}~%" (menu-name menu) (mapcar #'item-url (menu-items menu))))

(defun in-subtree (url subtree-url)
  (search subtree-url url))

(define-bknr-tag site-menu (&key config menu-name title container-class active-class inactive-class)
  (declare (ignore menu-name))
  (let* ((menu (bknr.impex:parse-xml-file
                #+cmu (ext:unix-namestring (merge-pathnames config *default-pathname-defaults*))
                #-cmu (namestring (probe-file (merge-pathnames config *default-pathname-defaults*)))
                *menu-def-classes*)))
    (html
     ((:div :class container-class)
      (when title
        (html ((:div :class "title") (:princ-safe title))))
      (dolist (item (menu-items menu))
	(let ((item-is-active (in-subtree (script-name*) (item-url item))))
	  (with-slots (url title active-image inactive-image) item
            (let ((link-url (format nil "~A~A" (website-base-href *website*) url)))
              (cond
                ((and active-image inactive-image)
                 (if item-is-active
                     (html ((:div :class active-class)
                            ((:img :src active-image :alt title))))
                     (html ((:div :class inactive-class)
                            ((:a :href link-url)
                             ((:img :src inactive-image :alt title)))))))
                (t
                 (if item-is-active
                     (html ((:div :class active-class)
                            (:princ-safe title)))
                     (html ((:div :class inactive-class)
                            ((:a :href link-url)
                             (:princ-safe title)))))))))))))))
