(in-package :bknr.bug-tracker)

(enable-interpol-syntax)

(defclass bug-tracker-handler (edit-object-handler)
  ())

(defmethod object-handler-get-object ((hander bug-tracker-handler))
  (let ((id-or-name (parse-url)))
    (when id-or-name
      (find-store-object id-or-name :class 'bug-tracker))))

(defmethod handle-object-form ((handler bug-tracker-handler) action (tracker (eql nil)))
  (let ((bug-trackers (all-bug-trackers)))
    (with-bknr-page (:title "Bug trackers")
      (:h2 "all bug-trackers")
      (:ul (dolist (bug-tracker bug-trackers)
	     (html (:li ((:a :href (format nil "/bug-tracker/~a"
					   (mailinglist-name bug-tracker)))
			 (:princ-safe (mailinglist-name bug-tracker))))))))))

(defmethod handle-object-form ((handler bug-tracker-handler) action tracker)
  (with-bknr-page (:title #?"bug-tracker for $((mailinglist-name tracker))")
    (when (admin-p (bknr-session-user))
      (html ((:a :href (format nil "/edit-bug-tracker/~a" (store-object-id tracker)))
	     "edit bug-tracker")))
    (bug-tracker-page :bug-tracker-id (store-object-id tracker))))

(defmethod file-bug-report ((handler bug-tracker-handler) tracker)
  (let ((user (bknr-session-user)))
    ;; XXX check user rights
    (with-query-params (name status priority description)
      (let ((bug-report (make-instance 'bug-report
                                       :tracker tracker
                                       :subject name
                                       :text description
                                       :status (make-keyword-from-string status)
                                       :priority (make-keyword-from-string priority)
                                       :owner user)))
	(bug-tracker-add-bug-report tracker bug-report)
	bug-report))))

(defmethod handle-object-form ((handler bug-tracker-handler) (action (eql :create-bug-report))
			       tracker)
  (let ((bug-report (file-bug-report handler tracker)))
    (redirect (format nil "/bug-report/~a" (store-object-id bug-report)))))

(defclass bug-report-handler (edit-object-handler)
  ())

(defmethod object-handler-get-object ((handler bug-report-handler))
  (let ((id-or-name (parse-url)))
    (when id-or-name
      (find-store-object id-or-name :class 'bug-report))))

(defmethod handle-object-form ((handler bug-report-handler) action (report (eql nil)))
  (redirect "/bug-tracker"))

(defmethod handle-object-form ((handler bug-report-handler) action report)
  (with-bknr-page (:title #?"bug-report")
    (when (or (equal (bknr-session-user)
		     (bug-report-handler report))
	      (admin-p (bknr-session-user)))
      (html ((:a :href (format nil "/edit-bug-report/~a" (store-object-id report)))
	     "edit bug-report")))
    (bug-page :bug-id (store-object-id report))))

(defmethod handle-object-form ((handler bug-report-handler) (action (eql :annotate))
			       report)
  (if report
      (let ((user (bknr-session-user)))
	(with-query-params (title text)
	  (let ((article (make-instance 'article
                                        :author user
                                        :subject title
                                        :text text)))
	    (if article
		(bug-report-add-annotation report article)
		(delete-object article))
	    (handle-object-form handler nil report))))
      (handle-object-form handler nil report)))

(defclass edit-bug-tracker-handler (bug-tracker-handler)
  ())

(defmethod handle-object-form ((handler edit-bug-tracker-handler) action
			       (bug-tracker (eql nil)))
  (let ((bug-trackers (all-bug-trackers)))
    (with-bknr-page (:title "Bug trackers")
      (:h2 "all bug-trackers")
      (:ul (dolist (bug-tracker bug-trackers)
	     (html (:li ((:a :href (format nil "/edit-bug-tracker/~a"
					   (mailinglist-name bug-tracker)))
			 (:princ-safe (mailinglist-name bug-tracker)))))))
      (:h2 "Create a new bug tracker")
      (bug-tracker-form))))

(defmethod handle-object-form ((handler edit-bug-tracker-handler)
			       (action (eql :create)) bug-tracker)
  (with-query-params (name email description)
    (if (and name email)
	(let ((bug-tracker (make-instance 'bug-tracker
                                          :name name
                                          :email email
                                          :description description)))
	  (redirect (format nil "/edit-bug-tracker/~a" (store-object-id bug-tracker))))
	(handle-object-form handler nil nil))))

(defmethod handle-object-form ((handler edit-bug-tracker-handler) (action (eql :create-bug-report))
			       tracker)
  (file-bug-report handler tracker)
  (redirect (format nil "/edit-bug-tracker/~a" (store-object-id tracker))))

(defmethod handle-object-form ((handler edit-bug-tracker-handler)
			       (action (eql :save))
			       tracker)
  (if (admin-p (bknr-session-user))
      (with-query-params (name email description)
	(change-slot-values tracker 'name name 'email email 'description description)
	(call-next-method))
      (with-bknr-page (:title #?"Edit bug tracker")
	(:p "You are not authorized to edit this bug tracker")
	((:a :href "/bug-tracker") "return to bug-tracker page"))))

(defmethod handle-object-form ((handler edit-bug-tracker-handler) action
			       bug-tracker)
  (with-bknr-page (:title #?"Edit bug tracker: $((mailinglist-name bug-tracker))")
    (bug-tracker-form :bug-tracker-id (store-object-id bug-tracker))))

(defclass edit-bug-report-handler (bug-report-handler)
  ())

(defmethod handle-object-form ((handler edit-bug-report-handler)
			       action (bug-report (eql nil)))
  (redirect "/edit-bug-tracker"))

(defmethod handle-object-form ((handler edit-bug-report-handler)
			       action bug-report)
  (with-bknr-page (:title #?"Edit bug report")
    (if bug-report
	(bug-form :bug-id (store-object-id bug-report))
	(redirect "/edit-bug-tracker"))))

(defmethod handle-object-form ((handler edit-bug-report-handler)
			       (action (eql :save))
			       report)
  (if (or (admin-p (bknr-session-user))
	  (equal (bknr-session-user)
		 (bug-report-handler report)))
      (with-query-params (name status priority description)
	(let ((status-kw   (make-keyword-from-string status))
	      (priority-kw (make-keyword-from-string priority)))
	  (if (eq status-kw :closed)
	      (change-slot-values report 'subject name
				  'text description
				  'status status-kw
				  'priority priority-kw
				  'last-modified (get-universal-time)
				  'closed (get-universal-time))
	      (change-slot-values report 'subject name
				  'text description
				  'status status-kw
				  'priority priority-kw
				  'last-modified (get-universal-time)))
	  (call-next-method)))
      (with-bknr-page (:title #?"Edit bug report")
	(:p "You are not the handler of this bug report")
	((:a :href (format nil "/bug-report/~a" (store-object-id report)))
	 "return to bug-report page"))))

(defmethod handle-object-form ((handler edit-bug-report-handler)
			       (action (eql :close))
			       report)
  (if (or (admin-p (bknr-session-user))
	  (equal (bknr-session-user)
		 (bug-report-handler report)))
      (progn
	(change-slot-values report 'closed (get-universal-time)
			    'status :closed
			    'last-modified (get-universal-time))
	(call-next-method))
      (with-bknr-page (:title #?"Edit bug report")
	(:p "You are not the handler of this bug report")
	((:a :href (format nil "/bug-report/~a" (store-object-id report)))
	 "return to bug-report page"))))

(defmethod handle-object-form ((handler edit-bug-report-handler)
			       (action (eql :reopen))
			       report)
  (if (or (admin-p (bknr-session-user))
	  (equal (bknr-session-user)
		 (bug-report-handler report)))
      (progn
	(change-slot-values report 'closed nil
			    'status :reopened
			    'last-modified (get-universal-time))
	(call-next-method))
      (with-bknr-page (:title #?"Edit bug report")
	(:p "You are not the handler of this bug report")
	((:a :href (format nil "/bug-report/~a" (store-object-id report)))
	 "return to bug-report page"))))

(defmethod handle-object-form ((handler edit-bug-report-handler)
			       (action (eql :delete))
			       report)
  (if (or (admin-p (bknr-session-user))
	  (equal (bknr-session-user)
		 (bug-report-handler report)))
      (progn
	(let ((tracker (bug-report-tracker report)))
	  (bug-tracker-remove-bug-report tracker report)
	  (delete-object report)
	  (redirect (format nil "/edit-bug-tracker/~a" (store-object-id tracker))))
        (with-bknr-page (:title #?"Edit bug report")
          (:p "You are not the handler of this bug report")
          ((:a :href (format nil "/bug-report/~a" (store-object-id report)))
           "return to bug-report page")))))

(defmethod handle-object-form ((handler edit-bug-report-handler)
			       (action (eql :handle))
			       report)
  (if (or (null (bug-report-handler report))
	  (admin-p (bknr-session-user)))
      (progn
	(change-slot-values report 'handler (bknr-session-user))
	(call-next-method))
      (with-bknr-page (:title #?"Edit bug report")
	(:p "You can not become the handler of this bug report")
	((:a :href (format nil "/bug-report/~a" (store-object-id report)))
	 "return to bug-report page"))))
      