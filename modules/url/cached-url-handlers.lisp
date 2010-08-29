(in-package :bknr-user)

;;; seen urls

;; xxx these are copied from base/rss/parse-xml.lisp

(defparameter *img-src-scanner*
  (create-scanner "(<ima?ge?[^>]+src=)\"([^\"]+)\"" :case-insensitive-mode t :multi-line-mode t))

(defparameter *a-href-scanner*
  (create-scanner "(<a[^>]+href=)\"([^\"]+)\"" :case-insensitive-mode t :multi-line-mode t))

(defparameter *link-href-scanner*
  (create-scanner "(<link[^>]+href=)\"([^\"]+)\"" :case-insensitive-mode t :multi-line-mode t))

(defun make-absolute-url (url base-url)
  (cond ((scan "^[a-zA-Z]+://" url) url)
	(t (render-uri (puri:merge-uris url base-url) nil))))

#+(or)
(defun cache-local-hrefs (data uri user depth &key (follow-links nil) (force nil))
  (let ((seen (make-hash-table :test #'equal)))
    (flet ((make-local-cache-link (target-string start end match-start match-end
						 reg-starts reg-ends)
	     (declare (ignore start end match-start match-end))
	     (let* ((match (subseq target-string
				   (svref reg-starts 1)
				   (svref reg-ends 1)))
		    (url (ignore-errors (make-absolute-url match uri)))
		    (rest (subseq target-string
				  (svref reg-starts 0)
				  (svref reg-ends 0))))
	       (if url
		   (let ((cached-url (or (gethash url seen)
					 (make-cached-url-from-url url
								   :parent-url url
								   :user user
								   :depth depth
								   :follow-links follow-links
								   :force force))))
		     (if cached-url
			 (progn (setf (gethash url seen) cached-url)
				(format nil "~a\"/cached-url/~a\""  rest
					(store-object-id cached-url)))
			 (format nil "~a\"~a\"" rest match)))
		   (format nil "~a\"~a\"" rest match)))))
      (setf data (regex-replace-all *img-src-scanner* data #'make-local-cache-link))
      (setf data (regex-replace-all *link-href-scanner* data #'make-local-cache-link))    
      (when follow-links
	(setf data (regex-replace-all *a-href-scanner* data #'make-local-cache-link)))
      data)))

#+(or)
(defun make-cached-url-from-url (url &key parent-url user (depth 1)
				 (force nil) (follow-links nil))
  (setf url (normalize-url url))
  (let ((cached-url (get-url-most-recent-cached-url url)))
    (if (and (not force) cached-url)
	cached-url
	(multiple-value-bind (data code headers uri)
	    (net.aserve.client:do-http-request url
	      :protocol :http/1.0
	      :headers (when parent-url `((:referrer . ,parent-url))))
	  (declare (ignore uri))
	  (if (= code 200)
	      (let ((content-type (or (cdr (assoc :content-type headers))
				      "text/html")))
		(when (and (string-beginning-with-p content-type "text/html")
			   (> depth 0))
		  (setf data (cache-local-hrefs data url user (1- depth)
                                                :follow-links follow-links
                                                :force force)))
		(let ((cached-url (make-instance 'cached-url
                                                 :url url
                                                 :content-type content-type
                                                 :type content-type
                                                 :timestamp (get-universal-time)
                                                 :parent-url parent-url
                                                 :cached-by user)))
		  (blob-from-string cached-url data)
		  cached-url))
	      nil)))))

(defclass cached-url-handler (object-handler)
  ((require-user-flag :initform :cache)))

(defmethod object-handler-get-object ((handler cached-url-handler))
  (find-store-object (parse-url) :class 'cached-url))

(defmethod handle-object ((handler cached-url-handler) (url (eql nil)))
  (with-bknr-page (:title "No such cached url")
    (:p "No such cached url")))

(defmethod handle-object ((handler cached-url-handler) url)
  (with-http-response (:content-type (cached-url-content-type url))
    (with-http-body ()
      (blob-to-stream url *html-stream*)
      (finish-output *html-stream*))))