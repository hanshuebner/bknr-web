(in-package :bknr.text)

(define-persistent-class billboard ()
  ((name :read
	 :index-type string-unique-index
	 :index-reader find-billboard
	 :index-values all-billboards)
   
   (description :update :initform nil)
   (articles :none :initform nil)
   (always-show-all :update :initform nil))
  (:documentation "message board with a list of articles and topics"))

(deftransaction billboard-add-article (billboard article)
  (setf (slot-value billboard 'articles)
	(push article (slot-value billboard 'articles))))

(deftransaction billboard-delete-article (billboard article)
  (setf (slot-value billboard 'articles)
	(delete article (billboard-articles billboard))))

(defun billboard-make-empty-description (billboard author)
  (let ((article (make-instance 'article
                                :author author)))
    (change-slot-values billboard 'billboard-description article)))

(defmethod billboard-articles ((billboard billboard) &key since new-for-user)
  (cond
    (since (remove since (slot-value billboard 'articles)
		   :test #'< :key #'article-time))
    (new-for-user (remove new-for-user (slot-value billboard 'articles)
			  :test #'(lambda (user article) (article-read article user))))
    (t (slot-value billboard 'articles))))

