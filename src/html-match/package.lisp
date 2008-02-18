(in-package :cl-user)

(defpackage :html-match
  (:use :common-lisp :cl-ppcre)
  (:export #:fail
	   #:no-bindings
	   #:html-match
	   #:html-search
	   #:+seq
	   #:+regex
	   #:+or
	   #:?_
	   #:html-pattern
	   #:html-patmatch
	   #:html-replace

	   #:hpc-pattern
	   #:hpc-patmatch
	   #:hpc-match
	   #:hpc-search))

