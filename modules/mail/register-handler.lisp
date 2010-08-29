(in-package :bknr.mail)

(enable-interpol-syntax)

(defclass register-handler (edit-object-handler)
  ()
  (:default-initargs :object-class 'registration-handler :query-function #'mail-handler-with-mail))

(defmethod authorized-p ((handler register-handler))
  t)

(defmethod handle-object-form ((handler register-handler) action reg-handler)
  (with-bknr-page (:title "register a user account")
    (:h2 "register a user account")
    ((:form :method "POST")
     (:table (:tr (:td "login")
		  (:td ((:input :type "text" :size 14 :name "login"))))
	     (:tr (:td "full-name")
		  (:td ((:input :type "text" :size 50 :name "full-name"))))
	     (:tr (:td "email")
		  (:td ((:input :type "text" :size 25 :name "email"))))
	     (:tr (:td "password")
		  (:td ((:input :type "password" :size 14 :name "password"))))
	     (:tr (:td "password (again)")
		  (:td ((:input :type "password" :size 14 :name "password2"))))
	     (:tr ((:td :colspan 2)
		   (submit-button "register" "register")))))))

(defmethod handle-object-form ((handler register-handler) action
			       (reg-handler (eql nil)))
  (with-bknr-page (:title "registration-handlers")
    (:ul (dolist (registration-handler
		   (remove 'registration-handler
			   (all-mail-handlers)
			   :test-not #'eql
			   :key #'type-of))
	   (html (:li ((:a :href (format nil "/register/~a"
					 (mail-handler-email registration-handler)))
		       (:princ-safe (mail-handler-email registration-handler)))))))))

(defun acceptable-mail-address-p (address)
  (scan #?r"^\S+@\S+\.[a-zA-Z]{2,4}$" address))

;; xxx registration-with-email and registration-with-login not defined
(defmethod handle-object-form ((handler register-handler)
			       (action (eql :register))
			       reg-handler)
  (with-query-params (login full-name email password password2)
    (with-bknr-page (:title "Register a user account")
      (cond ((not reg-handler)
	     (html (:h2 "No such registration-handler")))
	    ((not (and login full-name email password password2))
	     (html (:h2 "error while registering")
		   (:p "Please fill in all form fields")))
	    ((not (string-equal password password2))
	     (html (:h2 "error while registering")
		   (:p "Passwords do not match")))
	    ((or (find-user login)
		 (registration-with-email email)
		 (registration-with-login login))
	     (html (:h2 "error while registering")
		   (:p "Login is already used")))
	    ((not (acceptable-mail-address-p email))
	     (html (:h2 "error while registering")
		   (:p "Invalid email address")))
	    (t (let ((registration (make-registration
				    :login login
				    :full-name full-name
				    :password password
				    :email email)))
		 (if registration 
		     (let ((mail (make-instance 'mail
                                                :from (mail-handler-email reg-handler)
                                                :to (registration-email registration)
                                                :body "please reply"
                                                :subject (format nil "register on bknr (hash ~a)"
                                                                 (registration-hash registration)))))
		       (send-mail mail)
		       (html (:h2 "registration accepted")
			     (:p "You should receive an email asking for further confirmation")))
		     (html (:h2 "error while registering")
			   (:p "Could not create the registration")))))))))

(defmethod handle-object-form ((handler register-handler)
			       (action (eql :unsubscribe))
			       reg-handler)
  (with-query-params (email)
    (redirect (format nil "/unsubscribe?email=~a&action=search" email))))

(defmethod handle-object-form ((handler register-handler)
			       (action (eql :subscribe))
			       reg-handler)
  (with-query-params (email list)
    (with-bknr-page (:title "Create a subscription account")
      (cond ((not reg-handler)
	     (html (:h2 "No such registration-handler")))
	    ((not email)
	     (html (:h2 "error while registering")
		   (:p "email address not specified")))
	    ((not (acceptable-mail-address-p email))
	     (html (:h2 "error while registering")
		   (:p "Invalid email address")))
	    ((not (mailinglist-with-name list))
	     (html (:h2 "error while registering")
		   (:p "invalid mailing list name")))
	    (t (let* ((mailinglist (and list (mailinglist-with-name list)))
		      (registration (make-registration
				     :login email
				     :email email
				     :subscribe-mailinglist mailinglist))
		      (website-url (and mailinglist (mailinglist-website-url mailinglist))))
		 (if (admin-p (bknr-session-user))
		     (progn
		       (confirm-registration registration)
		       (html (:h2 "registration completed")
			     "Added mail address " (:princ-safe email)))
		     (progn
		       (send-mail (with-html-output-to-mail (:from (mail-handler-email reg-handler)
                                                             :to (registration-email registration)
                                                             :subject (format nil "registration on ~a (hash ~a)"
                                                                              website-url
                                                                              (registration-hash registration)))
				    (:html
				     (:body
				      (:p "You receive this message because you or someone else has requested that "
					  "you are added to our mailing list system.")
				      (:p "Please "
					  ((:a :href (registration-confirm-url registration)) "click this link")
					  " to activate your "
					  (if mailinglist
					      (html "mailing list subscription for "
						    (:princ-safe (mailinglist-name mailinglist))
						    " (" (:princ-safe (mailinglist-description mailinglist)) ")")
					      (html "registration"))
					  " on " (:princ website-url))))))
		       (html (:h2 "registration accepted")
			     (:p "You will receive an email asking for further confirmation"))))))))))

(defclass confirm-handler (edit-object-handler)
  ())

(defmethod authorized-p ((handler confirm-handler))
  t)

(defmethod object-handler-get-object ((handler confirm-handler))
  (registration-with-hash (parse-url)))

(defmethod handle-object-form ((handler confirm-handler)
			       (action (eql :confirm))
			       registration)
  (with-bknr-page (:title "Subscription confirmed")
    (if registration
	(handler-case
	    (let ((mailinglist (registration-subscribe-mailinglist registration)))
	      (confirm-registration registration)
	      (html (:p "Your email address " (:princ-safe (registration-email registration))
			" is now confirmed."))
	      (when mailinglist
		(html (:p "You have been subscribed to the mailing list "
			  (:princ-safe (mailinglist-name mailinglist))
			  " (" (:princ-safe (mailinglist-description mailinglist)) ")"))))
	  (error (e)
	    (html (:pre (:princ-safe (format nil "Cannot confirm registration for ~a:~%~a"
					     (registration-email registration)
					     e))))))
	(html (:p "No registration request has been found under this id.  Please try registering again.")))))

(defmethod handle-object-form ((handler confirm-handler)
			       action
			       registration)
  (with-bknr-page (:title "Subscription confirmation")
    (html ((:form :method "POST")
	   (:p "Please click the button to confirm your registration")
	   (submit-button "confirm" "confirm")))))

(define-bknr-webserver-module mailinglist-registration
    ("/register" register-handler)
  ("/confirm" confirm-handler))