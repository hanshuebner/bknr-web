(defpackage :bknr.mail
  (:use :cl
	:cl-user
	:cxml
	:cl-ppcre
	:cl-interpol
	:cl-smtp
	:bknr.utils
	:bknr.web
	:bknr.user
	:bknr.indices
	:bknr.datastore
	:bknr.impex
	:hunchentoot
	:puri
	:xhtml-generator)
  (:shadowing-import-from :cl-interpol quote-meta-chars)
  (:export #:subscription
	   #:subscription-mailinglist
	   #:subscription-user
	   #:subscription-type
	   #:mail
	   #:send-mail

	   #:mailinglist
	   #:mailinglist-name
	   #:mailinglist-email
	   #:mailinglist-description
	   #:mailinglist-keywords
	   #:mailinglist-subscriptions
	   #:mailinglist-with-name
	   #:mailinglist-with-email
	   #:get-keyword-mailinglists
	   #:all-mailinglists
	   #:mailinglist-users
	   #:mailinglist-send-mail
	   #:mailinglist-add-keyword
	   #:mailinglist-remove-keyword
	   #:tx-mailinglist-subscribe-user
	   #:mailinglist-send-mail))

(defpackage :bknr.mail.imap
  (:use :common-lisp
	#+cmu :ext #+sbcl :sb-ext)
  (:export 
   #:address-name
   #:address-additional
   #:address-mailbox
   #:address-host

   #:alter-flags
   #:close-connection
   #:close-mailbox
   #:copy-to-mailbox
   #:create-mailbox
   #:delete-letter
   #:delete-mailbox

   #:envelope-date
   #:envelope-subject
   #:envelope-from
   #:envelope-sender
   #:envelope-reply-to
   #:envelope-to
   #:envelope-cc
   #:envelope-bcc
   #:envelope-in-reply-to
   #:envelope-message-id

   #:expunge-mailbox
   #:fetch-field
   #:fetch-letter
   #:fetch-parts
   #:*imap-version-number*
   #:make-envelope-from-text
   #:mailbox-flags			; accessor
   #:mailbox-permanent-flags		; acc
   #:mailbox-list
   #:mailbox-list-flags
   #:mailbox-list-separator
   #:mailbox-list-name
   #:mailbox-message-count		; accessor
   #:mailbox-recent-messages		; ac
   #:mailbox-separator			; accessor
   #:mailbox-uidvalidity
   #:make-imap-connection
   #:make-pop-connection
   #:noop
   #:parse-mail-header
   #:top-lines				; pop only
   #:unique-id				; pop only

   #:po-condition
   #:po-condition-identifier
   #:po-condition-server-string
   #:po-error

   #:rename-mailbox
   #:search-mailbox
   #:select-mailbox))

(defpackage :bknr.smtp-server
  (:use :cl
	:cl-ppcre
	:cl-interpol)
  (:shadowing-import-from :cl-interpol quote-meta-chars))