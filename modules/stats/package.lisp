(defpackage :bknr.stats
  (:use :cl
	:cl-user
	:cxml
	:cl-gd
	:cl-ppcre
	:cl-interpol
	:hunchentoot
	:puri
	:bknr.utils
	:bknr.web
	:bknr.user
	:bknr.images
	:bknr.indices
	:bknr.datastore
	:bknr.events
	:xhtml-generator)
  (:export #:error-event-handler
	   #:log-handler
	   #:sessions-per-month-image-handler
	   #:hits-per-month-image-handler
	   #:sessions-per-day-image-handler
	   #:hits-per-day-image-handler
	   #:hits-per-hour-image-handler)
  (:shadowing-import-from :cl-interpol #:quote-meta-chars))