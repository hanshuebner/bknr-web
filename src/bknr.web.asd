;; -*- mode: lisp -*-
(in-package :cl-user)

(defpackage :bknr.web.system
  (:use :cl :asdf))

(in-package :bknr.web.system)

(defsystem :bknr.web
    :name "Baikonour - Base modules"
    :author "Hans Huebner <hans@huebner.org>"
    :author "Manuel Odendahl <manuel@bl0rg.net>"
    :version "0"
    :maintainer "Manuel Odendahl <manuel@bl0rg.net>"
    :licence "BSD"
    :description "Baikonour - Launchpad for LISP satellites - Base system"

    :depends-on (:cl-interpol
		 :cl-ppcre
		 :cl-gd
		 :alexandria
		 :md5
		 :cxml
		 :unit-test
		 :bknr.utils
		 :bknr.xml
		 :hunchentoot
                 :drakma
		 :xhtmlgen
		 :puri
		 :usocket
		 :bknr.datastore
		 :parenscript)

    :components ((:file "packages")
	       
		 (:module "sysclasses" :components ((:file "event")
						    (:file "user" :depends-on ("event"))
						    (:file "cron")
						    (:file "sysparam"))
			  :depends-on ("packages"))

		 (:module "htmlize" :components ((:file "hyperspec")
						 (:file "htmlize"
							:depends-on ("hyperspec")))
			  :depends-on ("packages"))

		 (:module "web" :components ((:file "site")
					     ;; data
					     (:file "host")
					     (:file "web-server-event"
						    :depends-on ("host"))
					     (:file "web-visitor"
						    :depends-on ("host"))

					     ;; web stuff
					     (:file "tag-functions")
					     (:file "web-macros"
						    :depends-on ("site"
								 "tag-functions"))
					     (:file "sessions"
						    :depends-on ("web-macros"
								 "site"))
					     (:file "authorizer"
						    :depends-on ("sessions"
								 "host"))
					     (:file "web-utils"
						    :depends-on ("web-macros"
								 "sessions"
								 "site"
								 "handlers"))
					     (:file "menu" :depends-on ("web-macros"))

					     ;; handlers
					     (:file "handlers"
						    :depends-on ("authorizer"
								 "web-macros"
								 "sessions"
								 "site"))

                                             (:file "handler-statistics-handler"
                                                    :depends-on ("handlers"))

					     (:file "template-handler"
						    :depends-on ("handlers"))

					     (:file "user-handlers"
						    :depends-on ("handlers"))
					     (:file "user-tags"
						    :depends-on ("handlers"))

					     (:file "tags"
						    :depends-on ("handlers"
								 "template-handler"
								 "site"
								 "web-utils")))
			  :depends-on ("sysclasses" "packages"))

		 (:module "frontend"
			  :depends-on ("packages" "web")
			  :serial t
			  :components ((:file "frontend-config")
				       (:file "frontend")))

		 (:module "rss" :components ((:file "rss")
                                             (:file "rss-handlers" :depends-on ("rss")))
			  :depends-on ("web"))

		 (:module "images" :components ((:file "image")
						
						(:file "image-tags" :depends-on ("image"))
						(:file "image-handlers"
						       :depends-on ("image-tags" "image"))
						(:file "imageproc-handler"
						       :depends-on ("image-handlers"))
						(:file "edit-image-handler"
						       :depends-on ("image-handlers"))
						(:file "import-images-handler"
						       :depends-on ("image-tags" "image"))
						(:file "session-image"))
			  :depends-on ("web"))))
