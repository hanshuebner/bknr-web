(in-package :bknr.smtp-server)

(enable-interpol-syntax)

(define-condition smtp-error (error)
  ((response-code :initarg :response-code :reader error-response-code)
   (message :initarg :message :reader error-message)))

(defun make-smtp-error (response-code message &rest args)
  (make-condition 'smtp-error :response-code response-code :message (apply #'format nil message args)))

(define-condition end-of-smtp-session (error)
  ())

(defclass smtp-client ()
  ((socket :initarg :socket :reader smtp-client-socket)
   (peer-id :initform nil :accessor smtp-client-peer-id)
   (from :initform nil :accessor smtp-client-from)
   (to :initform nil :accessor smtp-client-to)
   (message-lines :initform nil :accessor smtp-client-message-lines)))

(defmethod read-line-from-client ((client smtp-client) &optional eof-error-p eof-value)
  (regex-replace-all "[\\0\\n\\r]" (read-line (smtp-client-socket client) eof-error-p eof-value) ""))

(defmethod reply ((client smtp-client) response-code message &rest args)
  (format (smtp-client-socket client) "~a ~a~c~c" response-code (apply #'format nil message args) #\Return #\Linefeed)
  (finish-output (smtp-client-socket client)))

(defun parse-smtp-address-arg (address-string)
  (when address-string
    (multiple-value-bind
	  (match results)
	(scan-to-strings #?r"^<([^@>]+@[^@>]+)>$" address-string)
      (when match
	(aref results 0)))))

(defmethod reset ((client smtp-client))
  (setf (smtp-client-message-lines client) nil)
  (setf (smtp-client-from client) nil)
  (setf (smtp-client-to client) nil))

(defun scan-to-string-list (re string &rest args)
  (multiple-value-bind
	(match matches)
      (apply #'scan-to-strings re string args)
    (when match
	(mapcar #'(lambda (elem) (unless (equal "" elem) elem)) (coerce matches 'list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; smtp client handling

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-function-from-command (command-string)
  (find-symbol (string-upcase (regex-replace-all "\\s+" command-string "-"))
	       :bknr.smtp-server.commands))

(defmethod handle-session ((client smtp-client))
  (reply client 220 "bknr smtp service ready")
  (handler-case
      (loop
       (handler-case
	   (let ((input-line (read-line-from-client client)))
	     (unless input-line
	       (return-from handle-session))
	     (destructuring-bind
		   (&optional command args)
		 (scan-to-string-list #?r"^(?i)(helo|ehlo|rset|mail from|rcpt to|data|quit|vrfy)[: ]*(.*)$" input-line)
	       (if command
		   (let ((function (find-function-from-command command)))
		     (if function
			 (funcall function client args)
			 (reply client 502 "command ~a not implemented" command)))
		   (reply client 500 "unrecognized command"))))
	 (smtp-error (e)
	   (reply client (error-response-code e) (error-message e)))))
    (end-of-smtp-session (e)
      (declare (ignore e))
      (reply client 221 "thank you for your business"))
    (error (e)
      (reply client 451 "local error, cannot continue")
      (format t "; error ~a caught while handling smtp client~%" e))))
   
(defun handle-smtp-client (client-socket)
  (handle-session (make-instance 'smtp-client :socket client-socket)))

(defun smtp-server (&key (port 2525))
  (usocket:with-socket-listener (server-socket :local-port port :reuse-address t)
    (loop
       (let* ((client-socket (usocket:socket-accept server-socket)))
         (unwind-protect
              (handle-smtp-client client-socket)
           (close client-socket))))))
