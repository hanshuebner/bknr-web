(in-package :cl-user)

(defpackage :bknr.modules.system
  (:use :cl :asdf))

(in-package :bknr.modules.system)

(defsystem :bknr.modules
    :name "baikonour"
    :author "Hans Huebner <hans@huebner.org>"
    :author "Manuel Odendahl <manuel@bl0rg.net>"
    :version "0"
    :maintainer "Manuel Odendahl <manuel@bl0rg.net>"
    :licence "BSD"
    :description "Baikonour - Launchpad for LISP satellites - Application modules"

    :depends-on (:cl-interpol
		 :cl-ppcre
		 :cl-gd
		 :md5
		 :closer-mop
		 :cl-smtp
		 :cxml
		 :unit-test
		 :bknr.utils
		 :puri
		 :stem
		 :bknr.web
		 :parenscript)

    :components ((:file "packages")

		 (:module "class-browser" :components ((:file "class-browser"))
			  :depends-on ("packages"))
		 
		 (:module "text" :components ((:file "package")
					      (:file "article"
						     :depends-on ("package"))
					      (:file "vector-search"
						     :depends-on ("package"))
					      (:file "blog"
						     :depends-on ("article" "vector-search"))
					      #+(or)
					      (:file "billboard"
						     :depends-on  ("article"))
					      (:file "article-tags"
						     :depends-on ("article"))
					      (:file "paste-tags"
						     :depends-on ("article"))
					      (:file "blog-handlers"
						     :depends-on ("blog" "article-tags" "article-handlers"))
					      #+(or)
					      (:file "billboard-handlers"
						     :depends-on ("billboard" "article-tags"))
					      (:file "article-handlers"
						     :depends-on ("article" "article-tags"))
					      (:file "wiki-handlers"
						     :depends-on ("article-tags"))
					      (:file "htmlize-handler")
					      (:file "paste-handlers"
						     :depends-on ("paste-tags")))
			  :depends-on ("packages"))

		 #+(or)
		 (:module "feed" :components ((:file "feed")
					      (:file "feed-tags"
						     :depends-on ("feed"))
					      (:file "feed-handlers"
						     :depends-on ("feed" "feed-tags"))
					      (:file "edit-feed-handler"
						     :depends-on ("feed-handlers")))
			  :depends-on ("packages"))

		 (:module "url" :components ((:file "url")
					     (:file "url-tags"
						    :depends-on ("url"))
					     (:file "url-handlers"
						    :depends-on ("url" "url-tags"))
					     (:file "edit-url-handlers"
						    :depends-on ("url-handlers" "url-tags"))
					     (:file "cached-url-handlers"
						    :depends-on ("url-tags")))
			  :depends-on ("text" "packages"))

		 (:module "mail" :components ((:file "package")
					      (:file "mail"
						     :depends-on ("package"))
					      (:file "user-mail"
						     :depends-on ("mail"))
					      (:file "mailinglist"
						     :depends-on ("user-mail"))
					      (:file "registration"						    
						     :depends-on ("mailinglist"))
					      (:file "mailinglist-handlers"
						     :depends-on ("mailinglist"))
					      (:file "import-yahoo-group"
						     :depends-on ("mailinglist"))
					      (:file "register-handler"
						     :depends-on ("registration"))
					      (:file "smtp-server"
						     :depends-on ("package")))
			  :depends-on ("packages"))
	       
		 (:module "bug" :components ((:file "package")
					     (:file "bug"
						    :depends-on ("package"))
					     (:file "bug-tags"
						    :depends-on ("bug"))
					     (:file "bug-handlers"
						    :depends-on ("bug-tags")))
			  :depends-on ("text" "packages"))

		 (:module "quizz" :components ((:file "quizz")
					       (:file "quizz-tags"
						      :depends-on ("quizz"))
					       (:file "quizz-handlers"
						      :depends-on ("quizz-tags"))
					       (:file "edit-quizz-handlers"
						      :depends-on ("quizz-handlers" "quizz-tags")))
			  :depends-on ("packages"))

		 #+(or)	; uses bknr-datastore-actor, currently not supported
		 (:module "tamagotchi" :components ((:file "tamagotchi")
						    (:file "tamagotchi-tags"
							   :depends-on ("tamagotchi"))
						    (:file "tamagotchi-handlers"
							   :depends-on ("tamagotchi-tags")))
			 
			  :depends-on ("general" "web" "packages"))

		 #+(or)
		 (:module "track" :components ((:file "track")
					       (:file "media"
						      :depends-on ("track"))
					       (:file "track-tags"
						      :depends-on ("track"))
					       (:file "track-handlers"
						     
						      :depends-on ("track-tags"))
					       (:file "import-handler"
						      :depends-on ("track"))
					       (:file "dj"
						      :depends-on ("media")))
			  :depends-on ("packages"))

		 #+(or)
		 (:module "comics" :components ((:file "comics"))))
			  :depends-on ("packages"))
