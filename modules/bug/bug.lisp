(in-package :bknr.bug-tracker)

(define-persistent-class bug-tracker (mailinglist)
  ((bug-reports :update :initform nil)))

(defmethod print-object ((bt bug-tracker) stream)
  (format stream "#<BUG-TRACKER ID: ~A NAME: ~A>"
	  (store-object-id bt)
	  (mailinglist-name bt))
  bt)

(defun bug-tracker-p (obj)
  (typep obj 'bug-tracker))

(defun all-bug-trackers ()
  (store-objects-with-class 'bug-tracker))

(deftransaction bug-tracker-add-bug-report (bug-tracker bug-report)
  (push bug-report (bug-tracker-bug-reports bug-tracker)))

(deftransaction bug-tracker-remove-bug-report (bug-tracker bug-report)
  (setf (bug-tracker-bug-reports bug-tracker)
	(remove bug-report (bug-tracker-bug-reports bug-tracker))))

(define-persistent-class bug-report (article)
  ((tracker :read
	    :initform nil)
   (owner :update :initform nil
	  :index-type hash-index)
   (handler :update :initform nil
	    :index-type hash-index)
   
   (status :update
	   :initform :open
           :documentation "(or :open :closed :discarded :reopened)")
   (priority :update
             :initform :normal
             :documentation "(or :blocker :critical :major :normal :minor :trivial :enhancement)")
   (annotations :update :initform nil)
   (opened :read :initform (get-universal-time))
   (last-modified :update :initform nil)
   (closed :update :initform nil)))

(defun priority-to-num (priority)
  (position priority '(:enhancement :trivial :minor :normal :major :critical :blocker)))

(defmethod print-object ((br bug-report) stream)
  (format stream "#<BUG-REPORT ID: ~A STATUS: ~A>"
	  (store-object-id br)
	  (bug-report-status br))
  br)

(deftransaction bug-report-add-annotation (bug-report article)
  (setf (bug-report-last-modified bug-report) (article-time article))
  (push article (bug-report-annotations bug-report)))
