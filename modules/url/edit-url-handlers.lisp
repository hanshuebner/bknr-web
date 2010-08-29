(in-package :bknr-user)

(enable-interpol-syntax)

                                        ;(defclass edit-url-handler (edit-object-handler url-handler)
                                        ;  ((require-user-flag :initform :admin)))

(defclass submit-url-handler (form-handler)
  ())

(defmethod authorized-p ((handler submit-url-handler))
  (not (equal (bknr-session-user) (find-user "anonymous"))))

#+(or)
(defmethod handle-form ((handler submit-url-handler) action)
  (with-bknr-page (:title #?"submit url")
    (let ((keywords (keywords-from-query-param-list (query-param-list "keyword"))))
      (with-query-params (url title redirect)
	(html (:p "Drag this link to your bookmark bar: "
		  ((:a :href (format nil
				     "javascript:document.location.href=\"~a~a?title=\"+escape(document.title)+\"&url=\"+escape(document.location.href)+\"&redirect=1&keyword=fastsubmit\""
				     (website-url *website*)
				     "/submit-url"))
		   "bknr-url")))
	(submit-url-form :url url :title title :keywords keywords :redirect redirect)))))

#+(or)
(defmethod handle-form ((handler submit-url-handler) (action (eql :submit)))
  (with-query-params (title url description cache redirect)
    (let ((keywords (keywords-from-query-param-list (query-param-list "keyword"))))    
      (handler-case (progn
;;; verify form parameters
                      (ensure-form-field title)
                      (ensure-form-field url)
                      (setf url (normalize-url url))
                      (ensure-form-field keywords)
                      (if (and cache
                               (not (user-has-flag (bknr-session-user) :cache)))
                          (error (make-condition 'form-not-authorized-condition
                                                 :reason "You do not have the right to cache objects")))

                      (when cache
                        (make-cached-url-from-url url :user (bknr-session-user) :depth 1
                                                  :force nil))

                      (let ((url-obj (url-with-url url)))
                        (if url-obj
                            (store-object-add-keywords url-obj 'keywords keywords)
                            (setf url-obj (make-instance 'url :url url
                                                         :keywords keywords)))
		      
                        (let ((submission (make-instance 'url-submission
                                                         :url url-obj
                                                         :title title
                                                         :description description
                                                         :keywords keywords
                                                         :date (get-universal-time)
                                                         :submitter (bknr-session-user))))
                          (declare (ignore submission))
                          (redirect (if redirect url "/url")))))
        (form-field-missing-condition (e)
          (with-bknr-page (:title #?"submit url")
            ((:h2 :class "error")
             "Please fill field " (:princ-safe (form-field-missing-condition-field e)) "!")
            (submit-url-form :url url :title title :description description
                                                   :keywords keywords :cache cache :redirect redirect)))))))
