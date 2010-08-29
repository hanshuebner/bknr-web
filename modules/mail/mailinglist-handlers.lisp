(in-package :bknr.mail)

(enable-interpol-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass receive-mail-handler (page-handler)
  ())

(defmethod handle ((handler receive-mail-handler))
  (with-query-params (from to subject body)
    (if (and from to subject body)
	(let ((mail (make-instance 'mail
                                   :to to
                                   :from from
                                   :subject subject
                                   :body body)))
	  (with-bknr-page (:title "mail received")
	    (if (handle-incoming-mail mail)
		(html (:p "Mail was delivered successfully"))
		(html (:p "Mail could not be delivered")))))
	(with-bknr-page (:title "mail could not be received")
	  (:p "Can not receive empty mail")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass post-mailinglist-handler (admin-only-handler edit-object-handler)
  ()
  (:default-initargs :object-class 'mailinglist :query-function #'mailinglist-with-name))

(defmethod handle-object-form ((handler post-mailinglist-handler) action (mailinglist (eql nil)))
  (with-bknr-page (:title "Post message to mailing list")
    (if (class-instances 'mailinglist)
	(html
	 (:h1 "Select a mailinglist to post to")
	 (:ul
	  (loop for list in (class-instances 'mailinglist)
             do (html (:li (cmslink (format nil "/post-mailinglist/~A" (store-object-id list))
                                    (:princ-safe (mailinglist-name list)) "(" (:princ-safe (mailinglist-email list)) ")"))))))
	(html
	 (:h1 "No mailing lists found")
	 (cmslink "/edit-mailinglist" "Create List")))))

(defmethod handle-object-form ((handler post-mailinglist-handler)
			       action
			       (mailinglist mailinglist))
  (with-bknr-page (:title #?"Post message to mailing list $((mailinglist-name mailinglist))")
    (html ((:form :method "POST")
	   (:p "Subject: " ((:input :type "text" :size "50" :name "subject")))
	   (:p "Text" :br
	       ((:textarea :name "text" :rows "15" :cols "60") " "))
	   (submit-button "post" "post message")))))

(defmethod handle-object-form ((handler post-mailinglist-handler)
			       (action (eql :post))
			       mailinglist)
  (with-bknr-page (:title #?"Posting message to mailing list $((mailinglist-name mailinglist))")
    (html (:h2
	   (with-query-params (subject text)
	     (cond
	       ((not subject) (html "No subject specified"))
	       ((not text) (html "Text missing"))
	       (t (mailinglist-send-mail mailinglist
					 (make-instance 'mail :subject subject
                                                              :body text))
		  (html "Message posted"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass unsubscribe-handler (edit-object-handler)
  ())

(defmethod authorized-p ((handler unsubscribe-handler))
  t)

(defmethod object-handler-get-object ((handler unsubscribe-handler))
  (subscription-with-hash (parse-url)))

(defun html-subscription-info (user)
  (if (user-subscriptions user)
      (html (:h2 "subscriptions for user " (:princ-safe (user-login user)))
	    (dolist (subscription (user-subscriptions user))
	      (html (:princ-safe subscription)
		    " "
		    ((:a :href (subscription-unsubscribe-url subscription))
		     "unsubscribe")
		    :br)))
      (html (:h2 "no subscriptions for user " (:princ-safe (user-login user)) " found"))))

(defmethod handle-object-form ((handler unsubscribe-handler)
			       action
			       (subscription (eql nil)))
  (with-bknr-page (:title "Search subscription to remove")
    (html ((:form :method "POST")
	   "Enter email address: "
	   ((:input :type "text" :size "50" :name "email"))
	   (submit-button "search" "search")))))

(defmethod handle-object-form ((handler unsubscribe-handler)
			       (action (eql :search))
			       (subscription (eql nil)))
  (with-bknr-page (:title "Send unsubscribe information")
    (with-query-params (email)
      (let ((user (find-user email)))
	(if user
	    (if (admin-p (bknr-session-user))
		(html-subscription-info user)
		(progn
		  (html (:p "Sending unsubscribe information to " (:princ-safe (user-email user))))
		  (user-send-mail user (with-html-output-to-mail (:subject #?"mailing list subscription information")
					 (:html (html-subscription-info user))))))
	    (html (:h2 "email address " email " not on file")))))))

(defmethod handle-object-form ((handler unsubscribe-handler)
			       action
			       registration)
  (with-bknr-page (:title "Unsubscription confirmation")
    (html ((:form :method "POST")
	   (:p "Please click the button to confirm your unsubscription request.")
	   (submit-button "confirm" "confirm")))))

(defmethod handle-object-form ((handler unsubscribe-handler)
			       (action (eql :confirm))
			       subscription)
  (with-bknr-page (:title "Delete subscription")
    (delete-object subscription)
    (html (:p "The email address " (:princ-safe (user-email (subscription-user subscription)))
	      " is now unsubscribed from the mailing list "
	      (:princ-safe (mailinglist-name (subscription-mailinglist subscription)))
	      "."))))

(defclass edit-mailinglist-handler (admin-only-handler edit-object-handler)
  ()
  (:default-initargs :object-class 'mailinglist))

(defmethod handle-object-form ((handler edit-mailinglist-handler) (action (eql nil)) (mailinglist (eql nil)))
  (with-bknr-page (:title "Mailinglist Maintenance")
    (when (class-instances 'mailinglist)
      (html
       (:h1 "Select a mailinglist to edit")
       (:ul
	(loop for list in (class-instances 'mailinglist)
           do (html (:li (cmslink (format nil "/edit-mailinglist/~A" (store-object-id list))
                                  (:princ-safe (mailinglist-name list)) "(" (:princ-safe (mailinglist-email list)) ")")))))))
    (html
     (:h1 "Create a new list")
     ((:form :action "/edit-mailinglist" :method "post")
      (:table
       (:tr (:td "Email-Adress") (:td (text-field "email")))
       (:tr (:td "Name") (:td (text-field "name"))))
      (submit-button "create" "create")))))

(defmethod handle-object-form ((handler edit-mailinglist-handler) (action (eql :create)) (mailinglist (eql nil)))
  (with-query-params (email name)
    (with-bknr-page (:title "Create new mailinglist")
      (when (or (mailinglist-with-name name)
		(mailinglist-with-email email))
	(html
	 (:h1 "Duplicate name or email address")
	 "A mailing list with the name " (:princ-safe name) " or the email address " (:princ-safe email) " already exists")
	(return-from handle-object-form))
      (let ((list (make-mailinglist name email)))
	(html
	 (:h1 "Created mailinglist " (:princ-safe (mailinglist-name list)))
	 (cmslink (format nil "/edit-mailinglist/~A" (store-object-id list)) "[edit]"))))))

(defmethod handle-object-form ((handler edit-mailinglist-handler) (action (eql nil)) mailinglist)
  (with-bknr-page (:title #?"Edit mailinglist $((mailinglist-name mailinglist))")
    ((:table :border "1")
     (:tr (:td "Name") (:td (:princ-safe (mailinglist-name mailinglist))))
     (:tr (:td "Email") (:td (:princ-safe (mailinglist-email mailinglist)))))
    ((:form :action (script-name*) :method "post")
     (:table
      (:tr (:td "Subscribe email") (:td (text-field "email"))))
     (submit-button "subscribe" "subscribe"))))

(defmethod handle-object-form ((handler edit-mailinglist-handler) (action (eql :subscribe)) mailinglist)
  (with-query-params (email)
    (with-bknr-page (:title #?"Subscribe user $(email)")
      (let ((user (or (user-with-email (string-downcase email))
		      (prog1 
			  (make-user (md5-string (format nil "~A.~A" email (get-universal-time)))
				     :email email))
                      (html
                       (:h1 #?"created user with email $(email)")))))
	(mailinglist-subscribe-user mailinglist user)
	(html
	 (:h1 #?"Subscribed user with email $(email)"))))))

(define-bknr-webserver-module mailinglist
    ("/receive-mail" receive-mail-handler)
  ("/post-mailinglist" post-mailinglist-handler)
  ("/edit-mailinglist" edit-mailinglist-handler)
  ("/unsubscribe" unsubscribe-handler))