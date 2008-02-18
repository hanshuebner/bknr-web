(in-package :bknr.web)

(defun all-subclasses (class)
  "Return all subclasses of a given class.  Does not consider multiple inheritance."
  (append (mop:class-direct-subclasses class)
	  (loop for subclass in (mop:class-direct-subclasses class)
	      nconc (all-subclasses subclass))))

(defun subclasses-sorted-by-name (class-symbol)
  (sort (all-subclasses (find-class class-symbol))
	#'(lambda (a b) (string< (class-name a) (class-name b)))))
  
(defun sorted-subclass-names (class-symbol)
  "Return the names as string of all subclasses of the given class."
  (mapcar #'(lambda (class) (symbol-name (class-name class)))
	  (subclasses-sorted-by-name class-symbol)))

(defun seperator-height-string (event-time previous-event-time)
  "Calculate height in pixels of a seperator between two events.  One pixel equals 10 minutes."
  (let ((delta (- previous-event-time event-time)))
    (format nil "~a" (round (/ delta 600)))))

(defun selected-classes (query)
  "return all classes whose checkboxes are checked (show-class checkbox)"
  (loop
      for param in query
      when (string-equal "show-class" (car param))
      collect (find-class (find-symbol (cdr param) (find-package "bknr")))))

(defvar *default-event-classes-to-suppress* '(image-snapshot-event))

(defun default-selected-classes ()
  "Return all classes to display by default.  Classes listed in *default-event-classes-to-suppress* are supressed."
  (remove-if #'(lambda (class)
		 (find (class-name class) *default-event-classes-to-suppress*))
	     (all-subclasses (find-class 'event))))

(defun serve-event-class-documentation-request ()
  (with-bknr-page (:title "event class documentation")
    (html
     (:table
      (:tr (:th "name") (:th "documentation"))
      (dolist (event-class (subclasses-sorted-by-name 'event))
	(html
	 (:tr
	  (:td (:princ-safe (regex-replace "-event" (symbol-name (class-name event-class)) "")))
	  (:td (:princ-safe (documentation event-class 'structure))))))))))

(defmacro html-lognavi-checkbox (class-name &key checked)
  `(html
    ((:td :class "lognavi")
     ((:input :type "checkbox" :name "show-class" :value ,class-name ,@(if checked '(:checked "checked"))))
     (cmslink (format nil "event-log?show-only-class=~a" ,class-name)
       (:princ-safe (regex-replace ,class-name "-event$" ""))))))

(defun serve-event-log-request ()
  (with-query-params (
		      message
		      show-only-class
		      last-printed	;; Timestamp of last event printed (client-side session context)
		      frueher		;; Button "frÂüher" has been pressed, start printing at last-printed time.
		      print-hours	;; number of hours to search
		      print-count)	;; maximum number of events to print
    (when (and message (not (equal "" message)))
      (make-event 'message-event :from (bknr-session-user) :text message))
    ;; Parameter parsing, will move to with-query-params soon
    (if (and last-printed (not (equal "" last-printed)))
	(setf last-printed (parse-integer last-printed))
      (setf last-printed nil))
    (if (and frueher (not (equal "" frueher)))
	(setf frueher t)
      (setf frueher nil))
    (unless print-count
      (setf print-count "50"))
    (unless print-hours
      (setf print-hours "24"))
    (with-bknr-page (:title "event log")
      (let ((selected-classes (or (and show-only-class
				       (list (find-class (find-symbol show-only-class (find-package "bknr")))))
				  (selected-classes (request-query))
				  (mapcar #'find-class (get-user-preferences (bknr-session-user) :event-log-classes))
				  (default-selected-classes))))
	(unless show-only-class
	  (set-user-preferences (bknr-session-user) :event-log-classes (mapcar #'class-name selected-classes)))
	;; selected-classes contains the list of event classes to print.
	(html
	 ((:form :action "/event-log" :method "post")
	  ((:table :cellpadding "0" :cellspacing "0" :margin "0")
	   (html
	    (:tr
	     ;; Produce filter checkboxes - one checkbox for every subclass of 'event.
	     ((:td :class "lognavi" :valign "top" :width "640")
	      ((:table :cellpadding "0" :cellspacing "0" :border "0" :width "640" :height "100%")
	       (let ((event-class-names (sorted-subclass-names 'event)))
		 (loop 
		  for i from 0 by 5 to (length event-class-names)
		  do (html
		      (:tr
		       (loop 
			for class-name in (subseq event-class-names i (+ i 5))
			for checked = (find (find-class (find-symbol class-name (find-package "bknr"))) selected-classes)
			;; i'll learn how to properly use macros, someday
			do (if checked
			       (html-lognavi-checkbox class-name :checked t)
			       (html-lognavi-checkbox class-name :checked nil))))))))))
	    (:tr ((:td :class "lognavi") 
		  "count: " (html-text-input print-count 3)
		  " hours: " (html-text-input print-hours 3)
		  " " ((:input :type "submit" :name "filter" :value "filter"))
		  " " (cmslink ("event-class-documentation" :target "documentation") "&nbsp;documentation&nbsp;")))
	    #+(or)
	    (:tr ((:td :class "lognavi") "message: " ((:input :type "text" :size "80" :name "message")))))
	   ;; Query the database.
	   (when selected-classes
	     ;; Perform query only if any event subclass has been selected by the user.
	     (let ((cursor (make-instance 'timeline-cursor :timeline (the-event-timeline)
					  :backwards t 
					  :time (or (and frueher last-printed) 
						    (event-time (event-at-end (the-event-timeline))))))
		   (previous-event-time (get-universal-time))
		   (now (get-universal-time))
		   last-event-printed
		   (count 0))
	       (setf print-count (parse-integer print-count))
	       (setf print-hours (parse-integer print-hours))
	       (loop
		(let ((event (cursor-fetch cursor)))
		  (unless event
		    (return))
		  ;; The following 'when' clause should actually move into the cursor
		  (when (find (class-of event) selected-classes)
		    (html 
		     ;; Produce seperator
		     ;; Seperator's height is dynamically determined by seperator-height-string.
		     (:tr
		      ((:td :valign "top")
		       ((:img :src "img/pix.pif" :border "0" :width "2"
			      :height (seperator-height-string (event-time event) previous-event-time)))))
		     ;; Produce log entry line
		     (:tr
		      ((:td :class "logline" :valign "top" :width "640")
		       (print-as-html event *html-stream*))))
		    ;; Remember what we printed for seperator height calculation
		    (setf previous-event-time (event-time event))
		    (setf last-event-printed event)
		    (incf count)
		    (when (or (= print-count count)
			      (and last-event-printed
				   (< (* print-hours 60 60)
				      (- now (event-time last-event-printed)))))
		      (return)))))
	       (when show-only-class
		 (html ((:input :type "hidden" :name "show-only-class" :value show-only-class))))
	       (when last-event-printed
		 ;; Put timestamp of last event printed into the form as a hidden parameter until we have server-side session
		 ;; parameters.
		 (html ((:input :type "hidden" :name "last-printed" :value (format nil "~a" (event-time last-event-printed)))))))))))))))
