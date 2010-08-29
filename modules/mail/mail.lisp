(in-package :bknr.mail)

(enable-interpol-syntax)

(defparameter *smtp-server* "localhost")
(defparameter *default-from-address* "nobody@nothing.com")
(defparameter *default-subject* "no subject - automatically generated message")

(define-persistent-class mail ()
  ((to :read)
   (from :read
	 :initform *default-from-address*
	 :index-type hash-index)
   (subject :read :initform *default-subject*)
   (headers :read :initform nil)
   (received :read :initform nil)
   (in-reply :read :initform nil)

   (body :read :initform "<no message body>")))

(define-condition smtp-server-condition (error)
  ((server :initarg :server)
   (error-message :initarg :error-message)))

(defmethod print-object ((condition smtp-server-condition) stream)
  (with-slots (server error-message) condition
    (format stream "#<~a: error while talking to smtp server ~a: ~a>"
	    (class-name (class-of condition))
	    server error-message))
  condition)

(defmethod send-mail ((mail mail) &optional to)
  (send-letter *smtp-server*
	       (mail-from mail)
	       (or to (mail-to mail))
	       (mail-body mail)
	       :subject (mail-subject mail)
	       :headers (mail-headers mail)))

(defvar *nl-nl* #.(format nil "~C~C" #\Newline #\Newline))
(defvar *message-id-re* "<([^>]+)>")
(defvar *multipart-boundary-re* #?/multipart\/(\w+).+boundary=\"?([^\" ]+)\"?/)

(defmacro with-html-output-to-mail ((&rest mail-initargs &key headers &allow-other-keys) &rest body)
  (let ((new-headers (gensym)))
    `(let ((,new-headers (make-headers :content-type "text/html; charset=\"utf-8\""
                                       :mime-version "1.0")))
       (when ,headers
         (setf ,new-headers (append ,new-headers ,headers)))
       (make-instance 'mail
                      :headers ,new-headers
                      ,@(remove-keys '(:headers) mail-initargs)
                      :body (with-output-to-string (s)
                              (html-stream s ,@body))))))

;;; converted from macho (by Miles Egan)
(defun parse-header-string (headerstr)
  (let ((headers (make-hash-table :test #'equal)))
    (dolist (line (split #?/\n/ (regex-replace-all #?/\n\s+/ headerstr " ")))
      (loop for (keystr val) = (split #?/:\s+/ line :limit 2)
         for key = (make-keyword-from-string keystr)
         do (push val (gethash key headers))))
    headers))

(defun chop-multipart (stream content-type)
  "Look for a text section in a mime multipart message."
;;; XXX eof??
  (register-groups-bind (type boundary)
      (*multipart-boundary-re* content-type)
    (declare (ignore type))
    (loop with padding = (read-delimited stream (concatenate 'string "--" boundary))
       do (if (char= (peek-char nil stream) #\Newline)
              (progn (read-char stream)
                     (return (read-delimited stream boundary)))
              (let* ((headerstr (read-delimited stream *nl-nl*))
                     (headers (parse-header-string headerstr)))
                (if (scan "text" (first (gethash :content-type headers)))
                    (return (read-delimited stream boundary))))))))
  
(defun parse-mail (stream)
  (let* ((headerstr (read-delimited stream *nl-nl*))
	 (headers (parse-header-string headerstr)))
    (flet ((header (name)
	     (first (gethash name headers))))
      (let* ((content-type (header :content-type))
	     (body (if (scan "multipart" content-type)
		       (chop-multipart stream content-type)
		       (read-file stream))))
	(make-instance 'mail
                       :from (header :from)
                       :to (header :to)
                       :subject (header :subject)
                       :id (regex-replace-all
                            *message-id-re*
                            (header :message-id)
                            #?/\1/)
                       :in-reply (regex-replace-all
                                  *message-id-re*
                                  (first (if (header :in-reply-to)
                                             (split #?/\s+/ (header :in-reply-to))
                                             (last (split #?/\s+/ (header :references)))))
                                  #?/\1/)
                       :headers headers
                       :body body)))))


