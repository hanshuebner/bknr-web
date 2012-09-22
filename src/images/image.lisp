(in-package :bknr.images)

(enable-interpol-syntax)

(define-persistent-class store-image (owned-object blob)
  ((name :read
	 :index-type string-unique-index
	 :index-reader store-image-with-name
	 :index-values all-store-images)
   (directory :read :initform nil
                    :index-type hash-index :index-initargs (:test #'equal)
                    :index-reader store-images-with-directory
                    :index-keys all-store-image-directories)
   (keywords :update :initform nil
                     :index-type hash-list-index
                     :index-reader get-keyword-store-images
                     :index-keys all-image-keywords)
   
   (width :read :initform 0)
   (height :read :initform 0)))

(defmethod store-image-aspect-ratio ((image store-image))
  (/ (store-image-width image) (store-image-height image)))

(defmethod store-image-landscape-p ((image store-image))
  (< 1 (store-image-aspect-ratio image)))

(defmethod store-image-portrait-p ((image store-image))
  (> 1 (store-image-aspect-ratio image)))

(defmethod print-object ((object store-image) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S ID: ~A (~A x ~A)"
	    (store-image-name object)
	    (store-object-id object)
	    (store-image-width object)
	    (store-image-height object))))

(defun get-keywords-union-store-images (keywords)
  (reduce #'union
	  (mapcar #'get-keyword-store-images keywords)))

(defun get-keywords-intersection-store-images (keywords)
  (reduce #'intersection
	  (mapcar #'get-keyword-store-images keywords)))

(defun user-images (user)
  (get-keywords-intersection-store-images (list (make-keyword-from-string (user-login user))
						:user-image)))

(defun image-type-keyword (image)
  (if (keywordp (blob-mime-type image))
      (blob-mime-type image)
      (image-type-symbol (blob-mime-type image))))

(defmacro with-store-image ((var image) &body body)
  `(with-image-from-file (,var (blob-pathname ,image) (image-type-keyword ,image))
     ,@body))

(defmacro with-store-image* ((image) &body body)
  `(with-store-image (cl-gd:*default-image* ,image)
     ,@body))

(defun pathname-image-type (pathname)
  (case (find-symbol (string-upcase (pathname-type pathname)) :keyword)
    (:png :png)
    ((:jpg :jpeg) :jpg)
    (:gif :gif)
    (t (error "unknown image file type ~S" (pathname-type pathname)))))

(defun make-store-image (&key (image *default-image*)
			 name
                         original-pathname
			 (type :png type-provided-p)
			 directory keywords
                         (if-exists :error)
			 (class-name 'store-image)
			 initargs)
  (unless (scan #?r"\D" name)
    (error "invalid image name ~A, needs to contain at least one non-digit character" name))
  (when (and original-pathname
             (not type-provided-p))
    (setf type (pathname-image-type original-pathname)))
  (when-let (existing-image (store-image-with-name name))
    (ecase if-exists
      (:error
       (error "can't make image with name ~A, an image with this name already exists in the datastore" name))
      (:supersede
       (delete-object existing-image))
      (:kill
       (when (probe-file (blob-pathname existing-image))
         (delete-file (blob-pathname existing-image)))
       (delete-object existing-image))))
  (let ((store-image (apply #'make-instance
			    class-name
			    :name name
			    :type type
			    :directory directory
			    :keywords keywords
			    :width (image-width image)
			    :height (image-height image)
			    initargs)))
    (ensure-directories-exist (blob-pathname store-image))
    (ignore-errors (delete-file (blob-pathname store-image)))
    (if original-pathname
        (copy-file original-pathname (blob-pathname store-image))
        (apply #'cl-gd:write-image-to-file
               (blob-pathname store-image)
               :image image
               :type type
               (when (eq type :jpg)
                 (list :quality 95))))
    store-image))

(defmacro with-store-image-from-id ((var id) &rest body)
  (let ((image (gensym)))
    `(let ((,image (store-object-with-id ,id)))
       (unless ,image
         (error "image ~a not found" ,id))
       (with-store-image (,var ,image)
         ,@body))))

(defun find-image (image-id)
  (etypecase image-id
    (number (store-object-with-id image-id))
    (string (if (all-matches #?r"^\d+$" image-id)
		(store-object-with-id (parse-integer image-id))
		(store-image-with-name image-id)))
    (keyword (store-image-with-name (string-downcase (symbol-name image-id))))))

;;; import
(defgeneric import-image (pathname &key type name user keywords directory keywords-from-dir class-name initargs)
  (:documentation "Create blob from given source")
  (:method ((pathname pathname)
            &key type name user keywords directory (keywords-from-dir t) (class-name 'store-image) initargs)
    (unless (probe-file pathname)
      (error "file ~A does not exist" (namestring pathname)))
    (unless name
      (setq name (pathname-name pathname)))
    (unless (scan #?r"\D" name)
      (error "invalid image name ~A, needs to contain at least one non-digit character" name))
    (when (store-image-with-name name)
      (error "can't import image with name ~A, an image with this name already exists in the datastore" name))
    (let ((type (or type (pathname-type pathname))))
      (unless (keywordp type)
        (setf type (make-keyword-from-string type)))
      (with-image-from-file (image pathname type)
        ;; xxx not tx safe.
        (let ((store-image (apply #'make-instance 
                                  class-name
                                  :owner user
                                  :timestamp (get-universal-time)
                                  :name name
                                  :type (make-keyword-from-string type)
                                  :width (image-width image)
                                  :height (image-height image)
                                  :directory directory
                                  :keywords (if keywords-from-dir
                                                (append (mapcar #'make-keyword-from-string directory) keywords)
                                                keywords)
                                  initargs)))
          (blob-from-file store-image pathname)
          store-image))))
  (:method ((uri puri:uri) &rest args &key &allow-other-keys)
    (with-temporary-file (pathname)
      (multiple-value-bind
            (content status-code headers uri http-stream must-close status-text)
          (drakma:http-request uri :force-binary t)
        (declare (ignore status-code uri http-stream must-close status-text))
        (with-open-file (file pathname
                              :element-type '(unsigned-byte 8)
                              :direction :output
                              :if-exists :error)
          (write-sequence content file))
        (apply #'import-image
               pathname
               :type (bknr.utils:image-type-symbol (cdr (assoc :content-type headers)))
               args)))))

(defun image-directory-recursive (pathname)
  (remove-if-not #'pathname-content-type
                 (directory (merge-pathnames #P"**/*.*" pathname))))

(defun import-directory (pathname &key user keywords (spool *user-spool-directory-root*)
			 keywords-from-dir (class-name 'store-image))
  "Import all files from directory by giving them relative names"
  (let ((path-spool (cdr (pathname-directory spool))))
    (unless (subdir-p pathname spool)
      (error "imported directory ~a is not a subdir of spool ~a~%"
             pathname spool))
    (loop for file in (image-directory-recursive pathname)
       when (pathname-name file)
       collect (handler-case
                   (let ((image (import-image
                                 file
                                 :class-name class-name
                                 :keywords keywords
                                 :user user
                                 :directory (subseq (cdr (pathname-directory file))
                                                    (1+ (length path-spool)))
                                 :keywords-from-dir keywords-from-dir)))
                     (delete-file file)
                     (cons file image))
                 (error (e)
                   (cons file e)))
       when (and (not (pathname-name file))
                 (directory-empty-p file))
       do
       #+allegro
       (excl:delete-directory file)
       #+cmu
       (unix:unix-rmdir (namestring file))
       #+sbcl
       (sb-posix:rmdir (namestring file))
       #+openmcl
       (ccl::%rmdir (namestring file)))))
