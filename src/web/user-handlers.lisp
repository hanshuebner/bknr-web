(in-package :bknr.web)

(enable-interpol-syntax)

;;; links
(defmethod object-url ((user user))
  (format nil "/user/~a" (user-login user)))

(defmethod edit-object-url ((user user))
  (object-url user))

(defmethod html-link ((user user))
  (html
   (cmslink (object-url user)
     (:princ-safe (user-login user)))))

(defmethod html-edit-link ((user user))
  (html
   (cmslink (edit-object-url user)
    (:princ (format nil "edit ~a" (user-login user))))))

;;; handlers
  
(defclass logout-handler (page-handler)
  ())

(defmethod handle ((handler logout-handler))
  (setf (session-value 'bknr-session) nil)
  (format t "url: ~A referer: ~A~%" (query-param "url") (header-in :referer))
  (let ((url (or (query-param "url")
                 (header-in :referer))))
    (if url
        (redirect url)
        (progn (with-bknr-page (:title "logged out")
                 (html (:h2 "you are logged out")))))))

(defclass user-handler (edit-object-handler)
  ((require-user-flag :initform :admin)))

(defmethod authorized-p ((handler user-handler))
  (let* ((user (object-handler-get-object handler))
	 (web-user (bknr-session-user))
	 (action (query-param "action"))
	 (action-keyword (when action (make-keyword-from-string action))))
    (cond ((anonymous-p web-user) nil)
	  ((admin-p web-user) t)
	  ((and (or (null action-keyword)
		    (eql action-keyword :save))
		(equal user web-user))
	   t)
	  (t nil))))

(defmethod object-handler-get-object ((handler user-handler))
  (let ((id-or-name (parse-url)))
    (when id-or-name
      (find-store-object id-or-name :class 'user
			 :query-function #'find-user))))

(defmethod handle-object-form ((handler user-handler) action (user (eql nil)))
  (with-bknr-page (:title "Manage users")
    ((:table :border "1")
     (:tr (:th "Login")
	  (:th "Real name")
	  (:th "Privileges")
	  (:th "Last login"))
     (dolist (user (sort (remove-if-not #'user-editable-p (all-users))
			 #'string-lessp :key #'user-login))
       (html (:tr (:td ((:a :href (object-url user))
			(:princ-safe (user-login user))))
		  (:td (:princ-safe (user-full-name user)))
		  (:td (:princ-safe (format nil "~{~A~^, ~}" (user-flags user))))
		  (:td (:princ-safe (if (and (user-last-login user)
					     (plusp (user-last-login user)))
					(format-date-time (user-last-login user))
					"<never logged in>")))))))
    (:h2 "Create new user")
    (user-form)))

(defmethod handle-object-form ((handler user-handler) action (user user))
  (with-bknr-page (:title #?"$((class-name (class-of user))) $((user-login user))")
    #+(or) (bknr.images:user-image :user (user-login user))
    (user-form :user-id (store-object-id user))))

(defmethod handle-object-form ((handler user-handler) (action (eql :search)) user)
  (with-query-params (login)
    (redirect (format nil "/user/~A" login))))

(defmethod handle-object-form ((handler user-handler) (action (eql :save)) user)
  (unless user
    (setf user (bknr-session-user)))
  (when user
    (with-query-params (password password-repeat
			    full-name
			    (email (error "must provide email address")))
      (when (not (equal password password-repeat))
	(error "please enter the same new password twice"))
      (when password
	(set-user-password user password))
      (change-slot-values user 'email email 'full-name full-name)))

  (when (admin-p (bknr-session-user))
    (let* ((all-flags (all-user-flags))
	   (set-flags (keywords-from-query-param-list (query-param-list "flags")))
	   (unset-flags (set-difference all-flags set-flags)))
      (user-add-flags user set-flags)
      (user-remove-flags user unset-flags)))

  (call-next-method))

(define-condition unauthorized-error (simple-error)
  ()
  (:report "You are not authorized to perform this operation"))

(defmethod handle-object-form ((handler user-handler) (action (eql :delete)) user)
  (unless (admin-p (bknr-session-user))
    (error 'unauthorized-error))
  (when user
    (delete-user user))
  (redirect "/user"))

(defmethod handle-object-form ((handler user-handler) (action (eql :create)) user)
  (with-query-params (login email full-name password password-repeat)
    (if (and password
	     (not (equal password password-repeat)))
	(error "please enter the same password twice")
	(if login
	    (let* ((flags (keywords-from-query-param-list (query-param-list "keyword")))
		   (user (make-user login
				    :email email
				    :full-name full-name
				    :password password
				    :flags flags)))
	      (redirect (edit-object-url user)))
	    (error "please enter a login")))))

(define-bknr-webserver-module user
    ("/user" user-handler)
  ("/logout" logout-handler))
