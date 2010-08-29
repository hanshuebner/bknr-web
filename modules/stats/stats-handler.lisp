(in-package :bknr.stats)

(enable-interpol-syntax)

(defclass error-event-handler (object-handler)
  ((require-user-flag :initform :admin)))

(defmethod object-url ((event web-server-error-event))
  (format nil "/errors/~A" (store-object-id event)))

(defmethod html-link ((event web-server-error-event))
  (html
   (cmslink (object-url event)
     (:princ-safe (format nil "error in \"~a\"" (web-server-log-event-url event))))))

(defmethod object-handler-get-object ((handler error-event-handler))
  (let ((id (parse-handler-url handler)))
    (find-store-object id :class 'web-server-error-event)))

(defmethod handle-object ((handler error-event-handler)
			  (object (eql nil)))
  (let ((events (all-web-server-error-events)))
    (with-bknr-page (:title "error events")
      (:h1 "Error events")
      (:ul
       (dolist (event events)
	 (html (:li (:princ-safe (format-date-time (event-time event))) " "
		    (html-link event) ": "
		    (:princ-safe
		     (let ((errorstr (web-server-error-event-error event)))
		       (if (> (length errorstr) 25)
			   (concatenate 'string (subseq* errorstr 0 25) "...")
			   errorstr))))))))))

(defmethod handle-object ((handler error-event-handler)
			  (event web-server-error-event))
  (let ((url (web-server-log-event-url event)))
    (with-bknr-page (:title #?"Error in ${url}")
      (:h1 #?"Error in ${url}")
      (with-slots (time error referer url user host backtrace) event
	(html (:table (:tr (:td "Date") (:td (:princ-safe (format-date-time time))))
		      (:tr (:td "URL")
			   (:td (cmslink
				     (render-uri (merge-uris url (script-name*)) nil)
				  (:princ-safe url))))
		      (:tr ((:td :colspan "2") (:princ-safe error)))
		      (:tr ((:td :colspan "2") (:pre (:princ-safe backtrace))))))))))

(defclass stats-handler (object-handler)
  ())

(defmethod object-handler-get-object ((handler stats-handler))
  (let* ((date (parse-handler-url handler))
	 (date-components (mapcar #'(lambda (str) (parse-integer str :junk-allowed t))
				  (when date (split "-" date)))))
    (case (length date-components)
      (0 nil)
      (1 (list :year date-components
	       (apply #'make-yearly-stats date-components)))
      (2 (list :month date-components
	       (apply #'make-monthly-stats date-components)))
      (3 (list :day date-components
	       (apply #'get-daily-stats date-components)))
      (t nil))))

(defmethod handle-object ((handler stats-handler) (object (eql nil)))
  (redirect (format nil "/stats/~A"
		    (multiple-value-bind (seconds minute hour date month year)
			(decode-universal-time (get-universal-time))
		      (declare (ignore seconds minute hour date month))
		      year))))

(defmethod handle-object ((handler stats-handler) stats)
  (let ((type (first stats))
	(date (second stats))
	(web-stats (third stats)))
    (case type
      (:year (show-year-stats (first date) web-stats))
      (:month (show-month-stats (first date) (second date) web-stats))
      (:day (show-day-stats (first date) (second date) (third date) web-stats))
      ;; else redirect to year stats
      (t (handle-object handler nil)))))

(defun stats-to-html (stats)
  (html (:table (:tr (:td "Total hits")
		     (:td (:princ-safe (log-stats-hits stats))))
		(:tr (:td "Total sessions")
		     (:td (:princ-safe (log-stats-sessions stats))))
		(:tr (:td "Total unique urls")
		     (:td (:princ-safe (length (log-stats-hits-per-url stats)))))
		(:tr (:td "Hosts")
		     (:td (:princ-safe (length (log-stats-sessions-per-host stats)))))
		(:tr (:td "Users")
		     (:td (:princ-safe (length (log-stats-sessions-per-user stats))))))))

(defun stats-per-to-html (list name &key (key #'identity) (num 40) (hit-name "Hits")
			  make-href)
  (let ((total-hits (apply #'+ (mapcar #'cdr list))))
    (html ((:table :border 1 :cellpadding 2 :cellspacing 2)
	   (:tr (:td) (:td (:princ-safe hit-name)) (:td "Percent") (:td (:princ-safe name)))
	   (loop for i from 1
		 for (thing . hits) in (subseq* list 0 num)
		 for percent = (/ (round (* (/ hits total-hits) 100.0) 0.01) 100.0)
		 do (html (:tr (:td (:princ-safe i) ".")
			       (:td (:princ-safe hits))
			       (:td (:princ-safe percent) "%")
			       (:td (let ((foo (when thing (funcall key thing))))
				      (if make-href
					  (html (cmslink foo (:princ-safe foo)))
					  (html (:princ-safe foo))))))))))))

(defun show-year-stats (year stats)
  (with-bknr-page (:title #?"Statistics for year ${year}")
      (:h2 #?"Statistics for year ${year}:")
      (stats-to-html stats)
      (:h3 "Sessions per month:")
      ((:img :src (format nil "/sessions-per-month-stats/~A" year)))
      (:br)(:br)
      (:h3 "Hits per month:")
      ((:img :src (format nil "/hits-per-month-stats/~A" year)))
      (:br)(:br)
      (loop for month from 1 to 12
	    for month-name = (nth (1- month) '("jan" "feb" "mar" "apr" "may" "jun"
					       "jul" "aug" "sep" "oct" "nov" "dec"))
	    until (multiple-value-bind (seconds minute hour date m y)
		      (decode-universal-time (get-universal-time))
		    (declare (ignore seconds minute hour date))
		    (and (= year y)
			 (> month m)))
	    do (html (cmslink (format nil "/stats/~a-~a" month year)
		       (:princ-safe month-name)) " "))
      (:h3 "Hits per hour:")
      ((:img :src (format nil "/hits-per-hour-stats/~A" year)))
      (:br)(:br)
      (:h3 "Hits per URL:")
      (stats-per-to-html (log-stats-hits-per-url stats) "Url")
      (:h3 "Hits per Referer:")
      (stats-per-to-html (log-stats-hits-per-referer stats) "Referer" :make-href t)
      (:h3 "Hits per User-Agent:")
      (stats-per-to-html (log-stats-hits-per-user-agent stats) "User-Agent")
      (:h3 "Sessions per Host:")
      (stats-per-to-html (log-stats-sessions-per-host stats) "Host")
      (:h3 "Sessions per User:")
      (stats-per-to-html (log-stats-sessions-per-user stats) "User")))

(defun show-month-stats (month year stats)
  (with-bknr-page (:title #?"Statistics for month ${month} / ${year}")
    (:h2 #?"Statistics for month ${month} / ${year}:")
    (stats-to-html stats)
    (:h3 "Sessions per day:")
    ((:img :src (format nil "/sessions-per-day-stats/~A-~A" month year)))
    (:h3 "Hits per day:")
    ((:img :src (format nil "/hits-per-day-stats/~A-~A" month year)))
    (:h3 "Hits per hour:")
    ((:img :src (format nil "/hits-per-hour-stats/~A-~A" month year)))
    (:h3 "Hits per URL:")
    (stats-per-to-html (log-stats-hits-per-url stats) "Url")
    (:h3 "Hits per Referer:")
    (stats-per-to-html (log-stats-hits-per-referer stats) "Referer" :make-href t)
    (:h3 "Hits per User-Agent:")
    (stats-per-to-html (log-stats-hits-per-user-agent stats) "User-Agent")
    (:h3 "Sessions per Host:")
    (stats-per-to-html (log-stats-sessions-per-host stats) "Host")
    (:h3 "Sessions per User:")
    (stats-per-to-html (log-stats-sessions-per-user stats) "User")))

(defun show-day-stats (date month year stats)
  (with-bknr-page (:title #?"Statistics for ${date} / ${month} / ${year}")
    (:h2 #?"Statistics for ${date} / ${month} / ${year}:")
    (stats-to-html stats)
    (:h3 "Hits per hour:")
    ((:img :src (format nil "/hits-per-hour-stats/~A-~A-~A" date month year)))
    (:h3 "Hits per URL:")
    (stats-per-to-html (log-stats-hits-per-url stats) "Url")
    (:h3 "Hits per Referer:")
    (stats-per-to-html (log-stats-hits-per-referer stats) "Referer" :make-href t)
    (:h3 "Hits per User-Agent:")
    (stats-per-to-html (log-stats-hits-per-user-agent stats) "User-Agent")
    (:h3 "Sessions per Host:")
    (stats-per-to-html (log-stats-sessions-per-host stats) "Host")
    (:h3 "Sessions per User:")
    (stats-per-to-html (log-stats-sessions-per-user stats) "User")))

(defun log-graph (entries &key title (color '(0 0 255)) (column-width 50))
  (let* ((length (length entries))
	 (image-width (+ 20 (* length column-width)))
	 (image-height 220)
	 (column-top 60)
	 (column-bottom (- image-height 20))
	 (column-height (- column-bottom column-top))
	 (max (apply #'max (mapcar #'cdr entries))))
    (with-image* (image-width image-height)
      (allocate-color 230 230 230) ; grey background
      (let ((fg (apply #'allocate-color color))
	    (text (allocate-color 0 0 0)))
	(when title (draw-string 10 10 title :font :medium :color text))
	(draw-rectangle* 10 30 (- image-width 10) column-bottom :color text)
	(loop for (name . num) in entries
	      for i from 0
	      when (plusp num)
	      do (let ((x1 (+ 12 (* i column-width)))
		       (y1 (+ column-top (- column-height
					    (round (* column-height (/ num max))))))
		       (x2 (+ 10 (- column-width 2) (* i column-width)))
		       (y2 column-bottom))
		   (draw-rectangle* x1 y1 x2 y2 :filled t :color fg)
		   (draw-rectangle* x1 y1 x2 y2 :filled nil :color text)
		   (draw-string (+ 18 (* i column-width))
				(+ column-bottom 3) name :font :small :color text)
		   (when (> num 0)
		     (draw-string (+ 12 (* i column-width))
				  (- y1 15) (format nil "~A" num) :font :small :color text)))))
      (emit-image-to-browser *default-image* :gif :date (get-universal-time)))))

(defclass sessions-per-month-image-handler (stats-handler)
  ())

(defun make-month-log-graph (stats function &key title)
  (log-graph (mapcar #'cons '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
					  "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
			     (mapcar #'(lambda (s)
					 (if s (funcall function s) 0))
				     stats))
	     :title title
	     :column-width 30))

(defun make-day-log-graph (stats function &key title)
  (let ((entries (loop for s in stats
		       for i from 1
		       collect (cons (format nil "~A" i)
				     (if s (funcall function s) 0)))))
    (log-graph entries :title title :column-width 20)))

(defmethod handle-object ((handler sessions-per-month-image-handler) stats)
  (destructuring-bind (type date web-stats) stats
    (declare (ignore web-stats))
    (unless (eql type :year)
      (return-from handle-object))
    (let ((month-stats (nth-value 1 (apply #'make-yearly-stats date))))
      (make-month-log-graph month-stats #'log-stats-sessions
			    :title "Sessions per month"))))

(defclass hits-per-month-image-handler (stats-handler)
  ())

(defmethod handle-object ((handler hits-per-month-image-handler) stats)
  (destructuring-bind (type date web-stats) stats
    (declare (ignore web-stats))    
    (unless (eql type :year)
      (return-from handle-object))
    (let ((month-stats (nth-value 1 (apply #'make-yearly-stats date))))
      (make-month-log-graph month-stats #'log-stats-hits
			    :title "Hits per month"))))

(defclass sessions-per-day-image-handler (stats-handler)
  ())

(defmethod handle-object ((handler sessions-per-day-image-handler) stats)
  (destructuring-bind (type date web-stats) stats
    (declare (ignore web-stats))    
    (unless (eql type :month)
      (return-from handle-object))
    (let ((day-stats (nth-value 1 (apply #'make-monthly-stats date))))
      (make-day-log-graph day-stats #'log-stats-sessions :title "Sessions per day"))))

(defclass hits-per-day-image-handler (stats-handler)
  ())

(defmethod handle-object ((handler hits-per-day-image-handler) stats)
  (destructuring-bind (type date web-stats) stats
    (declare (ignore web-stats))
    (unless (eql type :month)
      (return-from handle-object))
    (let ((day-stats (nth-value 1 (apply #'make-monthly-stats date))))
      (make-day-log-graph day-stats #'log-stats-hits :title "Hits per day"))))

(defclass hits-per-hour-image-handler (stats-handler)
  ())

(defmethod handle-object ((handler hits-per-hour-image-handler) stats)
  (destructuring-bind (type date web-stats) stats
    (declare (ignore type date))
    (let ((entries (loop with hits-per-hour = (log-stats-hits-per-hour web-stats)
			 for hour from 0 to 23
			 collect (cons (format nil "~a" hour)
				       (or (cdr (assoc hour hits-per-hour)) 0)))))
      (log-graph entries
		 :title "Hits per hour"
		 :column-width 30))))

(define-bknr-webserver-module stats
    ("/error-event" error-event-handler)
  ("/stats" stats-handler)
  ("/sessions-per-month-stats" sessions-per-month-image-handler)
  ("/sessions-per-day-stats" sessions-per-day-image-handler)
  ("/hits-per-day-stats" hits-per-day-image-handler)
  ("/hits-per-hour-stats" hits-per-hour-image-handler))