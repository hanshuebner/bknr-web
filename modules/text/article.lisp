(in-package :bknr.text)

(define-persistent-class article (owned-object)
  ((author :read :initform nil)
   (time :update :initform (get-universal-time))
   (subject :update :initform "")
   (text :update :initform "")
   (read-by :update :initform nil)
   (search-vector :update :initform nil))
  (:documentation "generic article with subject and textual body"))

(defvar *template-expander* nil)

(defgeneric article-to-rss-item (article &key url))

(defmethod print-object ((object article) stream)
  (format stream "#<~a ID: ~S SUBJECT: ~S>"
	  (class-name (class-of object))
	  (store-object-id object)
	  (article-subject object))
  object)

;;; XXX new templater
#+(or)
(defmethod article-html-text ((article article))
  (if (eql #\< (aref (article-text article) 0))
      (let ((*template-expander* (or *template-expander* (make-instance 'template-expander))))
	(template-eval *template-expander*
		       (with-input-from-string (s (article-text article))
			 (parse s :compress-whitespace nil))
		       *current-template-environment*))
      (text-to-html (article-text article))))

(defmethod article-to-search-vector ((article article))
  (string-to-search-vector (concatenate 'string
					(article-subject article) " "
					(article-text article))))

(defmethod initialize-instance :after ((article article) &key)
  (setf (article-search-vector article)
	(article-to-search-vector article)))

(deftransaction index-article (article)
  (when article
    (setf (article-search-vector article)
	  (article-to-search-vector article))))

(defun search-articles (search-string articles &key (threshold 0.7))
  (search-vector (string-to-search-vector search-string) articles
		 :key #'article-search-vector
		 :threshold threshold))

(defmethod article-read ((article article) (user user))
  (find user (article-read-by article)))

(defun all-articles ()
  (store-objects-with-class 'article))

(deftransaction user-has-read-article (article user)
  "Put this user onto the list of users who have read this article"
  (unless (article-read article user)
    (push user (article-read-by article))))

#+(or)
(defmethod article-to-rss-item ((article article) &key (url (parse-uri "")))
  (let ((item-url (render-uri (puri:merge-uris (parse-uri (format nil "/article/~A" (store-object-id article)))
					       url) nil)))
    (make-instance 'rss-item
		   :about item-url
		   :title (xml-escape (article-subject article))
		   :link item-url
		   :desc (xml-escape (article-text article))
		   :creator (xml-escape (user-full-name (article-author article)))
		   :date (article-time article))))

(define-persistent-class keywords-article (article)
  ;; XXX  :keyword
  ((keywords :update :initform nil
	     :index-type hash-list-index
	     :index-reader get-keyword-articles
	     :index-keys all-article-keywords)))

(define-persistent-class snippet (keywords-article)
  ((keywords :update :initform nil
	     :index-type hash-list-index
	     :index-reader get-keyword-snippets)
   
   (expires :update :initform (get-universal-time))
   (layout :update :initform nil)))

(defmethod print-object ((snippet snippet) stream)
  (format stream "#<SNIPPET ID: ~a EXPIRES: ~a>" (store-object-id snippet)
	  (snippet-expires snippet))
  snippet)

(defun all-snippets ()
  (store-objects-with-class 'snippet))

(defun get-keywords-intersection-snippets (keywords)
  (reduce #'intersection (mapcar #'get-keyword-snippets keywords)))

(defun get-keywords-union-snippets (keywords)
  (reduce #'union (mapcar #'get-keyword-snippets keywords)))

(defun get-current-snippets (&key (time (get-universal-time)) keys)
  (remove-if #'(lambda (snippet)
		 (< (snippet-expires snippet) time))
	     (if keys
		 (get-keywords-intersection-snippets keys)
		 (all-snippets))))

(deftransaction snippet-change-expires (snippet expires)
  (setf (snippet-expires snippet) expires))

(define-persistent-class annotated-article (article)
  ((annotations :update :initform nil)))

(deftransaction annotate-article (article annotation)
  (push annotation (annotated-article-annotations article)))

(define-persistent-class paste (annotated-article keywords-article)
  ())

(defun all-pastes ()
  (store-objects-with-class 'paste))

(defun version-author (version)
  (second version))

(defun version-date (version)
  (third version))

(defun version-comment (version)
  (fourth version))

(defun version-text (version)
  (first version))

(defun make-version (text &key comment author date)
  (list text author date comment))

(define-persistent-class versioned-article (article)
  ((versions :update :initform nil)))

(defmethod versioned-article-newest-version ((article versioned-article))
  (first (versioned-article-versions article)))

(deftransaction article-add-version (article version)
  (push version (versioned-article-versions article))
  (setf (article-time article) (version-date version)))

(define-persistent-class wiki-article (versioned-article)
  ((keyword :read
	    :index-type unique-index
	    :index-reader wiki-article-with-keyword
	    :index-values all-wiki-articles
	    :index-keys wiki-keywords)))

(defun get-wiki-latest-revisions (&key (length 10))
  (subseq (sort (all-wiki-articles) #'> :key #'article-time) 0 length))
    
