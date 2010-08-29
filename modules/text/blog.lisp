(in-package :bknr.text)

(define-persistent-class blog-article (article rss-item)
  ((keywords :read :initform nil
	     :index-type hash-list-index)
   (blog :update :initform nil)))

(defmethod rss-item-channel ((article blog-article))
  (blog-article-blog article))

(define-persistent-class blog (rss-channel)
  ((bknr.rss::name :read
                   :index-type string-unique-index
                   :index-reader blog-with-name
                   :index-values all-blogs)
   (articles :update :initform nil)
   (owners :update :initform nil)))

(defmethod rss-channel-items ((blog blog) &key)
  (blog-articles blog))

(defmethod print-object ((object blog) stream)
  (format stream "#<~a ID: ~S NAME: ~S>"
	  (class-name (class-of object))
	  (store-object-id object)
	  (blog-name object))
  object)

(defmethod search-blog ((blog blog) search-string &key (threshold 0.7))
  (search-articles search-string (blog-articles blog) :threshold threshold))

(deftransaction blog-add-owner (blog owner)
  (pushnew (blog-owners blog) owner))

(deftransaction blog-add-article (blog article)
  (setf (blog-article-blog article) blog)
  (push article (blog-articles blog)))

(defmethod rss-item-title ((article article))
  (article-subject article))

(defmethod rss-item-description ((article article))
  (article-text article))

(defmethod rss-item-pub-date ((article article))
  (article-time article))