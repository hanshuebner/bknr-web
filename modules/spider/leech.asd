(in-package :cl-user)

(defpackage :leech.system
  (:use :cl :asdf))

(in-package :leech.system)

(defsystem :leech
    :name "leech"
    :author "Manuel Odendahl <manuel@bl0rg.net>"
    :version "0"
    :maintainer "Manuel Odendahl <manuel@bl0rg.net>"
    :licence "BSD"
    :description "leech - parallel HTTP downloader"

    :depends-on (:unit-test :aserve)

    :components ((:file "package")
		 (:file "leech" :depends-on ("package"))))
