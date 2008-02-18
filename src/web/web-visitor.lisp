(in-package :bknr.web)

(define-persistent-class web-visitor-event (event)
  ((user :read :relaxed-object-reference t)
   (session-id :read :initform nil)
   (host :read))
  (:documentation "web site being visited by a user"))

(define-persistent-class web-visitor-left-event (web-visitor-event)
  ()
  (:documentation "web visitor left the system"))

(defmethod event-argument ((event web-visitor-event))
  (format nil "~a ~a [~a]" (user-login (web-visitor-event-user event))
	  (host-name (web-visitor-event-host event))
	  (host-ip-address (web-visitor-event-host event))))

(defmethod print-object ((event web-visitor-event) stream)
  (print-unreadable-object (event stream :type t :identity t)
    (format stream "at ~A user ~A"
	  (format-date-time (event-time event))
	  (and (web-visitor-event-user event)
	       (user-login (web-visitor-event-user event))))
    (when (web-visitor-event-host event)
      (format stream " from ~a [~a]"
	      (host-name (web-visitor-event-host event))
	      (host-ip-address (web-visitor-event-host event)))))
  event)

#+(or)
(defmethod print-as-html ((event web-visitor-event) stream)
  (html-stream stream
	       (:princ-safe (format-date-time (event-time event) :show-year nil))
	       "&nbsp;["
	       (:princ-safe (event-class-name event))
	       "]&nbsp;"
	       (when (web-visitor-event-user event)
		 (html-link (web-visitor-event-user event)))
	       "&nbsp;from&nbsp;"
	       (when (web-visitor-event-host event)
		 (cmslink (format nil "host?host=~a" (host-ip-address (web-visitor-event-host event)))
		   (:princ-safe (host-name (web-visitor-event-host event)))))))

(defmethod as-xml ((event web-visitor-event))
  (bknr.events:generate-event-xml event
				  :user (when (web-visitor-event-user event)
					  (user-login (web-visitor-event-user event)))
				  :ip (host-ip-address (web-visitor-event-host event))
				  :hostname (host-name (web-visitor-event-host event))))

