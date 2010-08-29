(in-package :bknr.smtp-server.commands)

(enable-interpol-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; smtp commands

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod helo ((client smtp-client) args)
  (unless args
    (reply client 501 "missing parameter"))
  (when (smtp-client-peer-id client)
    (error (make-smtp-error 502 "HELO/EHLO command already seen")))
  (setf (smtp-client-peer-id client) args)
  (reply client 250 "hello ~a and welcome aboard!" args))

(defmethod noop ((client smtp-client) args)
  (when args
    (reply client 501 "bad parameter"))
  (reply client 250 "ok"))

(defmethod rset ((client smtp-client) args)
  (when args
    (reply client 501 "bad parameter"))
  (reset client)
  (reply client 250 "okay, transaction reset"))

(defmethod mail-from ((client smtp-client) args)
  (let ((from (parse-smtp-address-arg args)))
    (unless from
      (error (make-smtp-error 501 "missing parameter to MAIL FROM command")))
    (when (smtp-client-from client)
      (error (make-smtp-error 502 "MAIL FROM command already issued")))
    (setf (smtp-client-from client) from)
    (reply client 250 "sender accepted")))

(defmethod rcpt-to ((client smtp-client) args)
  (unless (smtp-client-from client)
    (error (make-smtp-error 502 "expecting MAIL FROM command")))
  (let ((to (parse-smtp-address-arg args)))
    (unless to
      (error (make-smtp-error 501 "bad parameter to RCPT TO command")))
    (if (mailinglist-with-email to)
	(progn
	  (push to (smtp-client-to client))
	  (reply client 250 "recipient accepted"))
	(reply client 550 "recipient ~a not known" to))))

(defmethod data ((client smtp-client) args)
  (when args
    (error (make-smtp-error 501 "bad argument(s) to DATA command")))
  (unless (and (smtp-client-from client)
	       (smtp-client-to client))
    (error (make-smtp-error 502 "can't handle DATA before both MAIL FROM and RCPT TO have been issued")))
  (reply client 250 "okay, give me the mail")
  (loop for line = (read-line-from-client client t)
	until (equal line ".")
	when (equal line "..")
	do (setf line ".")
	do (push (format nil "~a~%" line) (smtp-client-message-lines client)))
  (setf (smtp-client-message-lines client) (nreverse (smtp-client-message-lines client)))
  (handler-case
      (let ((mail (with-input-from-string (s (apply #'concatenate 'string (nreverse (smtp-client-message-lines client))))
		    (parse-mail s))))
	(handle-mail (mailinglist-with-email (smtp-client-to client))
		     mail)
	(reply client 250 "your mail has been processed"))
    (mail-parser-error (e)
      (reply client 451 "problem ~a delivering your email, can't deliver message" e)))
  (reset client))

(defmethod quit ((client smtp-client) args)
  (when args
    (error (make-smtp-error 501 "bad argument(s) to QUIT command")))
  (error (make-condition 'end-of-smtp-session)))

