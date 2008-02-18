(in-package :cl-user)

(defpackage :html-match.system
  (:use :cl :asdf))

(in-package :html-match.system)

(defsystem :html-match
    :name "html-match"
    :author "Manuel Odendahl <manuel@bl0rg.net>"
    :version "0"
    :maintainer "Manuel Odendahl <manuel@bl0rg.net>"
    :licence "BSD"
    :description "html-match - html pattern matcher"

    :depends-on (:unit-test :cl-ppcre)

    :components ((:file "package")
		 (:file "html-match" :depends-on ("package"))))
