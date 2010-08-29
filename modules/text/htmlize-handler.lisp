(in-package :bknr.text)

(enable-interpol-syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass htmlize-handler (object-handler)
    ((source-dir :initarg :source-dir :reader htmlize-handler-source-dir)
     (source-types :initarg :source-types :reader htmlize-handler-source-types
                   :initform '("lisp" "asd" "asdf" "cl")))))

(defmethod object-handler-get-object ((handler htmlize-handler))
  (let* ((url-path (bknr-url-path handler))
	 (source-path (htmlize-handler-source-dir handler))
	 (path (probe-file (merge-pathnames (if (and (> (length url-path) 0)
						     (eql (char url-path 0) #\/))
						(subseq url-path 1)
						url-path)
					    source-path))))
    (if (and path (subdir-p path source-path))
	path
	(htmlize-handler-source-dir handler))))

(defmethod relative-link-to-file ((handler htmlize-handler) filepath)
  (let ((len (length (pathname-directory (htmlize-handler-source-dir handler)))))
    (make-pathname :directory (cons :relative (subseq (pathname-directory filepath) (+ 2 len)))
		   :name (pathname-name filepath)
		   :type (pathname-type filepath))))

(defmethod handle-object ((handler object-handler) path)
  (unless path
    (error "no path to generic object handler given, missing specialization?"))
  (if (pathname-name path)
      (with-bknr-page (:title (pathname-name path))
	(htmlize-file path *html-stream*))
      (with-bknr-page (:title #?"Source directory: $((namestring path))")
	(:ul (dolist (file (directory path))
	       (let ((file-path (relative-link-to-file handler file)))
		 (when (or (not (pathname-name file-path))
			   (member (pathname-type file-path)
				   (htmlize-handler-source-types handler)
				   :test #'string-equal))
		   (html (:li ((:a :href (format nil "~a/~a"
						 (page-handler-prefix handler)
						 (namestring file-path)))
			       (:princ-safe (namestring file-path))))))))))))
