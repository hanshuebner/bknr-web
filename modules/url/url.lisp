(in-package :bknr-user)

(define-persistent-class url ()
  ((url :read
	:index-type string-unique-index
	:index-reader url-with-url
	:index-values all-urls)
   (keywords :update :initform nil
	     :index-type hash-list-index
	     :index-reader get-keyword-urls
	     :index-keys all-url-keywords)))

(define-persistent-class url-submission ()
  ((submitter :read
	      :index-type hash-index
	      :index-reader get-user-url-submissions)
   (url :read
	:index-type hash-index :index-initargs (:test #'equal)
	:index-reader get-url-url-submissions)
   (keywords :read :initform nil
	     :index-type hash-list-index
	     :index-reader get-keyword-url-submissions)
	     
   (title :read)
   (description :read)
   (date :read :initform (get-universal-time))))

(defmethod print-object ((object url) stream)
  (format stream "#<~a ID: ~A ~S>"
	  (class-name (class-of object))
	  (store-object-id object)
	  (url-url object))
  object)

(defun normalize-url (url &key (protocol "http"))
  (unless (scan "^[a-zA-Z]+://" url)
    (setf url (concatenate 'string protocol "://" url)))
  (setf url (regex-replace-all "(\\?.*)$" url ""))
  (render-uri (parse-uri url) nil))

(defun get-keywords-union-urls (keywords)
  (reduce #'union (mapcar #'get-keyword-urls keywords)))

(defun get-keywords-intersection-urls (keywords)
  (reduce #'intersection (mapcar #'get-keyword-urls keywords)))

(defun url-submissions (url)
  (sort (get-url-url-submissions url) #'> :key #'url-submission-date))

(defun url-latest-submission (url)
  (first (url-submissions url)))

(defun url-latest-submission-date (url)
  (url-submission-date (url-latest-submission url)))

(defmethod url-submission-to-rss-item ((submission url-submission))
  (let ((url (url-submission-url submission)))
    (make-instance 'rss-item
		   :about (url-url url)
		   :link (url-url url)
		   :title (url-submission-title submission)
		   :desc (url-submission-description submission)
		   :date (url-submission-date submission))))

(define-persistent-class cached-url (blob)
  ((url :read
	:index-type hash-index :index-initargs (:test #'equal)
	:index-reader get-url-cached-urls
	:index-values all-cached-urls)
   (parent-url :read :initform nil
	       :index-type hash-index :index-initargs (:test #'equal))
   
   (content-type :read)
   (cached-by :read :initform nil)))

(defmethod print-object ((obj cached-url) stream)
  (format stream "#<~a ID: ~a, ~A>"
	  (class-name (class-of obj))
	  (store-object-id obj)
	  (cached-url-url obj))
  obj)

(defun get-url-most-recent-cached-url (url)
  (let ((urls (sort
	       (get-url-cached-urls url)
	       #'> :key #'blob-timestamp)))
    (first urls)))
