;;;# Introduction

;;; This is a simple stress tester for HTTP servers. It simulates a
;;; "normal" web user by following links in the documents it receives
;;; from the webserver.

;;; The stress tester needs `aserve' (or `portableaserve') and `cl-ppcre'.

(in-package :cl-user)

(use-package :cl-ppcre)
(use-package :puri)

;;;# Helper functions

;;; We need to choose a random link from a list of links later on.
(defun random-elt (choices)
  (when choices
    (elt choices (random (length choices)))))

;;; This is quite a hacked function to test if two uris are equal.
(defun uri-equal (uri1 uri2)
  (flet ((normalize-uri (uri)
	   (typecase uri
	     (string (render-uri (parse-uri uri) nil))
	     (uri (render-uri uri nil))
	     (t (error "~a is not an URI" uri)))))
    (string-equal (normalize-uri uri1)
		  (normalize-uri uri2))))


;;;# Extraction functions

;;; These regexps match IMG uris and A HREF uris respectively.
(defparameter *img-uri-regexp*
  (create-scanner "<\\s*img\\s+[^>]*src=\"([^\"]+)\""
		  :case-insensitive-mode t
		  :multi-line-mode t))

(defparameter *href-uri-regexp*
  (create-scanner "<\\s*[a-zA-Z]+\\s+[^>]*href=\"([^\"]+)\""
		  :case-insensitive-mode t
		  :multi-line-mode t))

(defparameter *link-regexp*
  (create-scanner "(<\\s*[^>]*)(href|src)=\"([^\"]+)\""
		  :case-insensitive-mode t
		  :multi-line-mode t))

(defun remove-common-components (list1 list2)
  (do ()
      ((or (null list1)
	   (null list2)
	   (not (equal (first list1) (first list2))))
       (values list1 list2))
    (pop list1)
    (pop list2)))

(defun make-subdir-string (i)
  (apply #'concatenate 'string (make-list i :initial-element "../")))

(defun dir-uri-p (uri)
  (let ((path (uri-path uri)))
    (eql (char path (1- (length path))) #\/)))

(defun join (joiner strings)
  (if (null strings)
      ""
      (with-output-to-string (s)
       (do ((l (cdr strings) (cdr l))
            (str (car strings) (car l)))
           ((null l)
            (write-string str s))
         (write-string str s)
         (write-string joiner s)))))

(defun replace-uris (html base-uri)
  (flet ((replace-uri (target-string start end match-start match-end reg-starts reg-ends)
	   (declare (ignore start end))
	   (flet ((reg (i)
		    (subseq target-string (svref reg-starts i)
			    (svref reg-ends i))))
	     (let ((tag (reg 0))
		   (href-or-src (reg 1))
		   (match (reg 2)))
	       (setf match (regex-replace-all " " match "%20"))
	       (let ((uri (merge-uris match base-uri)))
		 (if (and (string-equal (uri-scheme uri) "http")
			    (string-equal (uri-host uri)
					  (uri-host base-uri)))
		     (multiple-value-bind (orig-path uri-path)
			 (remove-common-components (or (uri-parsed-path base-uri) '(:absolute))
						   (or (uri-parsed-path uri) '(:absolute)))
		       (concatenate 'string tag href-or-src "=\""
				    (if orig-path
					(make-subdir-string (1- (length orig-path)))
					"")
				    (join "/" uri-path) "\""))
		     (subseq target-string match-start match-end)))))))
    (regex-replace-all *link-regexp* html #'replace-uri)))

;;; Extract the URIs matching `regexp' from the HTML document `html',
;;; returning only URIs which are of the `http' scheme, and which are
;;; on the same host as `base-uri'. Relative URIs are resolved against
;;; `base-uri'.
(defun extract-uris (regexp html base-uri)
  (when (stringp base-uri)
    (setf base-uri (parse-uri base-uri)))
  (let (res)
    (do-register-groups (match) (regexp html)
      (setf match (regex-replace-all " " match "%20"))
      (let ((uri (merge-uris match base-uri)))
	(when (and (string-equal (uri-scheme uri) "http")
		   (string-equal (uri-host uri)
				 (uri-host base-uri)))
	  (push uri res))))
    (nreverse (remove-duplicates res))))

;;; The two functions extract IMG uris and HREF uris from a HTML document.
(defun extract-img-uris (html base-uri)
  (extract-uris *img-uri-regexp* html base-uri))

(defun extract-href-uris (html base-uri)
  (extract-uris *href-uri-regexp* html base-uri))

;;;# Stress tester

;;; A stress tester has a list of starting urls, as well as a
;;; probability to return to a link that has been already visited.
(defclass stress-tester ()
  ((start-urls :initarg :start-urls :reader st-start-urls)
   (history :accessor st-history :initform nil)
   (cookie-jar :initform (make-instance 'net.aserve.client:cookie-jar)
	       :accessor st-cookie-jar)
   (get-imgs :initform t :initarg :get-imgs :reader st-get-imgs)
   (seen-probability :initform 0.1
		     :initarg :seen-probability
		     :reader st-seen-probability)))

;;; This structure records the result of page retrieval by the stress tester.
(defstruct hit
  uri code duration length)

;;; This method retrieves `uri' and stores the results in the history
;;; of the stress-tester. If the content-type of the answer is
;;; text/html, it tries to retrieve all linked images, and then
;;; returns all availables links in the document.
(defmethod st-get-uri ((st stress-tester) uri)
  (let ((start-time (get-internal-real-time)))
    (multiple-value-bind (body code headers)
	(net.aserve.client:do-http-request uri
	  :cookies (st-cookie-jar st))

      (let ((duration (/ (- (get-internal-real-time) start-time)
			 internal-time-units-per-second))
	    (length (length body))
	    (content-type (cdr (assoc "content-type" headers :test #'string-equal))))
	(format *trace-output* "uri: ~A, code: ~A, type: ~A~%" uri code content-type)	
	(push (make-hit :uri uri :code code :duration duration :length length)
	      (st-history st))
	(force-output)
	(when (and (= code 200)
		   content-type
		   (scan "text/html" content-type))
	  (when (st-get-imgs st)
	    (dolist (img-uri (remove-duplicates (extract-img-uris body uri)
						:test #'uri-equal))
	      (st-get-uri st img-uri)))
	  (remove-duplicates (extract-href-uris body uri) :test #'uri-equal))))))

(defmethod st-touch-uri ((st stress-tester) uri)
  (format *trace-output* "uri: ~A~%" uri)
  (force-output)
  (multiple-value-bind (body code headers)
      (net.aserve.client:do-http-request uri
	:cookies (st-cookie-jar st))
    (let ((content-type (cdr (assoc "content-type" headers :test #'string-equal))))
      (when (and (= code 200)
		 content-type
		 (scan "text/html" content-type))
	(nconc (if (st-get-imgs st) (extract-img-uris body uri) nil)
	       (extract-href-uris body uri))))))

;;; Is this uri already in our history?
(defmethod seen-uri-p ((st stress-tester) uri)
  (when (find uri (st-history st) :key #'hit-uri :test #'uri-equal)
    t))

;;; Return a random uri from `uris', with a probability of
;;; `st-seen-probability' to choose an URI which has already been
;;; visited. If URIs is empty, return a URI from `start-urls'.
(defmethod random-uri ((st stress-tester) uris)
  (let (seen-uris unseen-uris)
    (dolist (uri uris)
      (if (seen-uri-p st uri)
	  (push uri seen-uris)
	  (push uri unseen-uris)))
    (format t "seen-uris: ~A~%unseen-uris: ~A~%" seen-uris unseen-uris)
    (if (and seen-uris unseen-uris)
	(if (<= (random 1.0) (st-seen-probability st))
	    (random-elt seen-uris)
	    (random-elt unseen-uris))
	(random-elt (or seen-uris unseen-uris (st-start-urls st))))))

;;; Visit `num' pages starting from the start urls of `st'. On each
;;; page, visit all images, and follow a random link according to the rules above.
(defmethod run ((st stress-tester) &optional (num 100))
  (setf (st-history st) nil
	(st-cookie-jar st) (make-instance 'net.aserve.client:cookie-jar))
  (do ((i 0 (1+ i))
       (uri (random-elt (st-start-urls st))))
      ((= i num) (setf (st-history st) (nreverse (st-history st))) t)
    (let ((uris (st-get-uri st uri)))
      (setf uri (random-uri st uris)))))

;;; Print some results about the stress test.
(defmethod analyse-history ((st stress-tester))
  (flet ((extremum (values fun)
	   (float (reduce fun values :initial-value (first values))))
	 (average (values)
	   (float (/ (reduce #'+ values) (length values)))))
    (let* ((hits (length (st-history st)))
	   (durations (mapcar #'hit-duration (st-history st)))
	   (average-duration (average durations))
	   (min-duration (extremum durations #'min))
	   (max-duration (extremum durations #'max))

	   (lengths (mapcar #'hit-length (st-history st)))
	   (average-length (average lengths))
	   (min-length (extremum lengths #'min))
	   (max-length (extremum lengths #'max)))

      (format t "Hits: ~A~%~%" hits)
      (format t "Average duration: ~A s~%" average-duration)
      (format t "Maximal duration: ~A s~%" max-duration)
      (format t "Minimal duration: ~A s~%~%" min-duration)
      (format t "Average length: ~A b~%" average-length)
      (format t "Maximal length: ~A b~%" max-length)
      (format t "Minimal length: ~A b~%~%" min-length)
      (format t "Hits per second: ~A~%" (/ 1 average-duration))
      (format t "Total throughput: ~A b~%" (* hits average-length))
      (format t "Average throughput: ~A b/s~%~%" (/ average-length average-duration))

      (format t "Five longest duration uris:~%")
      (dolist (hit (subseq (sort (copy-list (st-history st))
				 #'> :key #'hit-duration) 0 5))
	(format t "~A: ~A s~%" (hit-uri hit) (float (hit-duration hit))))
      (format t "~%")

      (format t "Five longest body uris:~%")
      (dolist (hit (subseq (sort (copy-list (st-history st))
				 #'> :key #'hit-length) 0 5))
	(format t "~A: ~A b~%" (hit-uri hit) (float (hit-length hit)))))))

;;; Start parallel stress testers.
(defun parallel-stress-test (start-urls num-clients num-pages &key (seen-probability 0.1))
  (let (processes)
    (unwind-protect
	 (let* ((sts (loop for i from 0 to num-clients
			   collect (make-instance 'stress-tester
						  :start-urls start-urls
						  :seen-probability seen-probability)))
		(i 0))
	   (setf processes (mapcar #'(lambda (st)
				       (acl-compat.mp:process-run-function
					(format nil "stress-tester-~A" (incf i))
					#'(lambda () (run st num-pages))))
				   sts))
	   (do () ((every #'(lambda (proc) (not (acl-compat.mp:process-active-p proc)))
			  processes) nil)
	     (acl-compat.mp:process-allow-schedule))
	   (setf (st-history (first sts)) (mapcan #'st-history sts))
	   (first sts))
      (dolist (proc processes)
	(process-kill proc)))))

(defclass touch-all-spider (stress-tester)
  ())

(defmethod run ((st touch-all-spider) &optional (num nil))
  (let ((seen-hash (make-hash-table :test #'equal))
	(urls (mapcar #'parse-uri (st-start-urls st))))
    (dolist (url urls)
      (setf (gethash (render-uri url nil) seen-hash) t))
    (setf (st-cookie-jar st) (make-instance 'net.aserve.client:cookie-jar))
    (do ()
	((or (null urls)
	     (if num (<= num 0))) t)
      (let* ((uri (pop urls))
	     (page-urls (st-touch-uri st uri)))
	(when num
	  (decf num))
	(dolist (url page-urls)
	  (unless (gethash (render-uri url nil) seen-hash)
	    (setf (gethash (render-uri url nil) seen-hash) t)	    
	    (push url urls)))))))

(defclass save-all-spider (touch-all-spider)
  ((dir :initarg :dir :reader save-all-spider-dir)))

(defmethod initialize-instance :after ((spider save-all-spider) &rest initargs)
  (declare (ignore initargs))
  (ensure-directories-exist (save-all-spider-dir spider)))

(defmethod st-touch-uri ((spider save-all-spider) uri)
  (format *trace-output* "uri: ~A~%" uri)
  (force-output)
  (multiple-value-bind (body code headers)
      (net.aserve.client:do-http-request uri
	:cookies (st-cookie-jar spider))
    (let ((content-type (cdr (assoc "content-type" headers :test #'string-equal))))
      (with-simple-restart (continue "continue spidering") 
	(let* ((path (or (uri-parsed-path uri) `(:absolute "--index--")))
	       (dir (cons :relative (subseq path 1 (1- (length path)))))
	       (filename (car (last path)))
	       (pathname (merge-pathnames
			  (make-pathname :name filename :directory dir)
			  (save-all-spider-dir spider))))
	  (ensure-directories-exist (make-pathname :name nil :defaults pathname))
	  (when (probe-file pathname)
	    (warn "~a exists, overwriting" pathname))
	  (with-open-file (s pathname :if-exists :supersede :if-does-not-exist :create :direction :output)
	    (write-sequence (replace-uris body uri) s))))
      (when (and (= code 200)
		 content-type
		 (scan "text/html" content-type))
	(nconc (if (st-get-imgs spider) (extract-img-uris body uri) nil)
	       (extract-href-uris body uri))))))
			 
