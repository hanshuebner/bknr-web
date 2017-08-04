
(in-package :cl-user)

(defpackage :bknr.sysparams
  (:use :cl :cl-user :bknr.indices :bknr.datastore)
  (:export #:sysparam
	   #:set-sysparam))

(defpackage :bknr.htmlize
  (:use :cl :cl-user :bknr.utils)
  (:export #:to-html
	   #:htmlize-file
	   #:htmlize-string
	   #:htmlize))

(defpackage :bknr.cron
  (:use :cl :cl-user :bknr.utils :bknr.indices :bknr.datastore)
  (:export #:make-cron-job
	   #:cron-job-with-name
	   #:start-cron))

(defpackage :bknr.rss
  (:use :cl :cl-user :cl-ppcre :bknr.utils :bknr.xml :puri :hunchentoot :bknr.datastore :bknr.indices :cxml)
  (:export ;; channel

	   #:rss-channel
	   #:find-rss-channel
	   #:make-rss-channel
	   #:rss-channel-cleanup
	   #:rss-channel-about
	   #:rss-channel-title
	   #:rss-channel-link
	   #:rss-channel-desc
	   #:rss-channel-image
	   #:rss-channel-textinput
	   #:rss-channel-items
           #:rss-channel-archive
           #:rss-channel-archived-months
	   #:rss-channel-xml

	   ;; item
	   #:rss-item
	   #:rss-item-channel
	   #:rss-item-published
	   #:rss-item-pub-date
	   #:rss-item-title
	   #:rss-item-link
	   #:rss-item-description
	   #:rss-item-author
	   #:rss-item-category
	   #:rss-item-comments
	   #:rss-item-enclosure
	   #:rss-item-guid
	   #:rss-item-source
	   #:rss-item-encoded-content))

(defpackage :bknr.events
    (:use :cl
	  :xhtml-generator
	  :bknr.utils
	  :bknr.datastore
	  :cl-ppcre)
    (:documentation "events framework, currently exports all defined symbols until refactoring")
    (:export #:event
	     #:event-time
	     #:event-handler
	     #:event-argument
	     #:event-class-name

	     #:make-event
	     #:find-events
	     #:all-events

	     #:handle-event
	     #:generate-event-xml))

(defpackage :bknr.user
  (:use :cl
	:cl-user
	:cl-interpol
	:cl-ppcre
	:md5
	:bknr.datastore
	:bknr.indices
	:bknr.utils
	:bknr.events
	:xhtml-generator)
  (:shadowing-import-from :cl-interpol #:quote-meta-chars)
  (:export #:user

	   #:user-full-name
	   #:user-last-login
	   #:user-email
	   #:user-login
	   #:user-password
	   #:user-flags
	   #:user-preferences
	   #:user-subscriptions
	   #:user-editable-p

           ;; Export slot names so that derived classes can overload
           ;; slots (e.g. to add XML impex attributes)
           #:login
           #:flags
	   #:email
	   #:full-name
           #:last-login
	   #:password
           #:preferences
           #:subscriptions
           #:mail-error

	   #:find-user
	   #:user-with-email
	   #:admin-p
	   #:anonymous-p

	   #:user-has-flag
	   #:user-add-flags
	   #:user-remove-flags
	   #:all-user-flags
	   #:define-user-flag

	   #:user-reachable-by-mail-p
	   #:user-mail-error-p
	   #:verify-password
	   #:user-disabled
	   #:user-preferences
	   #:user-preference
	   #:set-user-preference
	   #:all-users
	   #:get-flag-users
	   #:make-user
	   #:delete-user
	   #:set-user-password

	   #:set-user-last-login

	   #:owned-object
	   #:owned-object-owner
	   #:store-objects-owned-by
           #:store-object-owners

	   #:message-event))

(defpackage :bknr.web
  (:use :cl
	:cl-user
	:cl-gd
	:cl-interpol
	:cl-ppcre
	:alexandria
	:hunchentoot
	:cxml-xmls
	:xhtml-generator
	:puri
	:md5
	:bknr.datastore
	:bknr.indices
	:bknr.impex
	:bknr.utils
	:bknr.xml
	:bknr.events
	:bknr.user)
  (:shadowing-import-from :cl-interpol #:quote-meta-chars)
  (:shadowing-import-from :hunchentoot #:host)
  (:shadowing-import-from :alexandria #:array-index)
  (:export #:*user*
	   #:with-http-request
	   #:with-http-body
           #:with-xml-response
	   #:request-variable
	   #:with-query-params
	   #:define-bknr-tag
	   #:with-bknr-page
	   #:cmslink

           #:bknr-acceptor

	   #:web-server-log-event-referer
	   #:web-server-log-event-url
	   #:web-server-log-event-user-agent

	   #:web-visitor-event-host
	   #:web-visitor-event-session-id
	   #:web-visitor-event-user

	   #:web-server-error-event
	   #:web-server-error-event-error
	   #:all-web-server-error-events

	   ;; web-utils
	   #:*upload-file-size-limit*
	   #:all-request-params
	   #:request-uploaded-files
	   #:request-uploaded-file
           #:query-params
	   #:query-param
	   #:query-param-list
	   #:cookie-value
	   #:http-error
	   #:keywords-from-query-param-list
	   #:html-quote
	   #:parse-url
	   #:parse-uri
	   #:text-to-html
	   #:make-wiki-hrefs
	   #:html-link
	   #:html-edit-link
	   #:object-url
	   #:edit-object-url
	   #:xmls-emit
	   #:emit-html
	   #:make-self-reference-url
	   #:html-warn
	   #:redirect
	   #:redirect-uri
	   #:emit-html
	   #:error-404
	   #:encode-urlencoded
	   #:submit-button
	   #:text-field
	   #:textarea-field
	   #:checkbox-field
	   #:select-box
	   #:date-field
	   #:parse-date-field
	   #:keyword-choose-dialog
	   #:navi-button
	   #:with-http-response

	   #:upload
	   #:upload-name
	   #:upload-pathname
           #:upload-original-filename
	   #:upload-size
	   #:upload-content-type

           #:with-image-from-upload
           #:with-image-from-upload*

	   #:bknr-url-path

           #:with-json-response

	   ;; templates
	   #:expand-template
	   #:expand-variables
	   #:get-template-var
	   #:with-template-vars
	   #:emit-template-node
	   #:user-error
	   #:find-template-pathname
	   #:initial-template-environment
	   #:with-tag-expanders
	   #:emit-tag-children
     #:emit-tag-child

	   #:*html-variables*
           #:*template-dtd-catalog*

	   ;; handlers
           #:bknr-dispatch
	   #:parse-handler-url
	   #:*website*
	   #:website
	   #:website-name
           #:website-host
	   #:website-authorizer
	   #:website-show-page
	   #:website-show-error-page
	   #:website-handler-definitions
	   #:website-admin-navigation
	   #:website-navigation
	   #:website-menu
	   #:website-url
	   #:website-session-info
           #:website-base-href
           #:website-make-path
	   #:website-cachable-handlers
	   #:host
	   #:publish-site
	   #:publish-handler
	   #:unpublish

	   #:handler-matches-p
	   #:handle-object
	   #:handle-object-form
	   #:handle-form
	   #:object-handler-object-class
	   #:object-handler-get-object
           #:object-handler-query-function
	   #:require-user-flag

	   #:bknr-authorizer
	   #:authorize

	   #:handle
	   #:object-handler
	   #:edit-object-handler
	   #:template-handler
	   #:template-handler-destination
	   #:cachable-handler
	   #:page-handler
	   #:page-handler-prefix
	   #:page-handler-site
	   #:page-handler-url	   
           #:authorize
	   #:authorized-p
	   #:admin-only-handler
	   #:prefix-handler
	   #:form-handler
	   #:login-handler
	   #:logout-handler
	   #:redirect-handler
	   #:directory-handler
	   #:file-handler

           #:backbone-handler

	   #:keyword-handler
	   #:keywords-handler

	   #:rss-handler

	   #:define-bknr-webserver-module

	   #:ensure-form-field
	   #:form-field-missing-condition
	   #:form-field-missing-condition-field

	   #:handler-path
	   #:handler-max-age
	   #:decoded-handler-path

	   ;; misc tags xxx should be revised xxx
	   #:next-days-list
	   #:previous-days-list
	   #:reset-results

	   ;; choice (html menus)
	   #:make-choice
	   #:choice-link
	   #:choice-title
	   #:choice-submenu

	   ;; object-list-handler
	   #:object-list-handler
	   #:object-list-handler-get-objects
	   #:object-list-handler-title
	   #:object-list-handler-show-object-xml
	   #:object-date-list-handler
	   #:object-date-list-handler-grouped-objects
	   #:object-date-list-handler-date

	   ;; xml-object-handler
	   #:xml-object-handler
	   #:xml-object-handler-show-object
	   #:xml-object-list-handler
	   #:xml-image-browser-handler

	   ;; blob-handler
	   #:blob-handler
   
	   ;; sessions
	   #:bknr-session
	   #:bknr-session-user
	   #:bknr-session-start-time
	   #:bknr-session-last-used
	   #:bknr-session-values
	   #:bknr-session-host
   
	   #:host-name
	   #:bknr-session
	   #:*session*
	   #:anonymous-session

	   ;; site
	   #:*default-billboard*
	   #:*thumbnail-max-height*
	   #:*thumbnail-max-width*
	   #:*user-spool-directory-root*

	   ;; import-handler
	   #:import-handler
	   #:import-handler-spool-dir
	   #:import-handler-spool-files
	   #:import-handler-import-files
	   #:import-handler-import-pathname))

(defpackage :bknr.web.frontend
  (:use :cl :bknr.web)
  (:export #:frontend-already-running
	   #:front-end-running-p
	   #:start-frontend
	   #:stop-frontend
	   ))

(defpackage :bknr.images
  (:use :cl
	:cl-user
	:cl-gd
	:cl-interpol
	:cl-ppcre
	:alexandria
	:hunchentoot
	:puri
	:xhtml-generator
	:bknr.rss
	:bknr.web
	:bknr.datastore
	:bknr.indices
	:bknr.utils
	:bknr.user)
  (:shadowing-import-from :cl-interpol #:quote-meta-chars)
  (:shadowing-import-from :bknr.indices #:array-index)
  (:export #:imageproc
           #:*current-image*
	   #:define-imageproc-handler
	   #:image-handler			; plain images only
	   #:imageproc-handler			; image with processing

	   #:banner				; tag to display the site's banner image
	   #:user-image
	   #:user-images

	   #:parse-color
	   #:get-keyword-store-images
	   #:get-keywords-intersection-store-images
   
	   #:emit-image-to-browser
	   #:image-collection
	   #:image-keyword-choose-dialog
	   #:image-thumbnail-page

	   #:store-image-with-name

	   #:store-image
	   #:make-store-image
	   #:with-store-image
	   #:with-store-image*
	   #:with-store-image-from-id
	   #:image-type-keyword

	   #:store-image-name
	   #:store-image-height
	   #:store-image-width
	   #:store-image-aspect-ratio
	   #:store-image-keywords

	   #:emit-image-to-browser

	   #:import-image))

(defpackage :bknr.site-menu
  (:use :cl
	:cl-user
	:cxml
	:bknr.web
	:bknr.impex
	:hunchentoot
	:xhtml-generator))
