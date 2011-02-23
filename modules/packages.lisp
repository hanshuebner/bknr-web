(defpackage :bknr-user
  (:use :cl
	:cl-user
	:cxml
	:cl-ppcre
	:cl-interpol
	:hunchentoot
	:puri
	:bknr.rss
	:bknr.utils
	:bknr.web
	:bknr.user
	:bknr.images
	:bknr.indices
	:bknr.datastore
	:bknr.impex
	:bknr.events
	:xhtml-generator)
  (:shadowing-import-from :cl-interpol quote-meta-chars)
  (:shadowing-import-from :parenscript while in create))
