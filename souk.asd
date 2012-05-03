;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:souk-asd
  (:use :cl :asdf))

(in-package :souk-asd)

(defsystem souk
    :name "souk"
    :version "0.0.1"
    :maintainer ""
    :author ""
    :licence ""
    :description "souk"
    :depends-on (:weblocks-elephant :cl-who :lisp-magick :cl-fad)
    :components ((:file "souk")
		 (:module conf
		  :components ((:file "stores"))
		  :depends-on ("souk"))
		 (:module src
		  :components ((:file "init-session")
			       (:file "parser")
			       (:module models
				:components ((:file "qlist")
					     (:file "line-item")
					     (:file "single-item" :depends-on ("line-item"))
					     (:file "bundle" :depends-on ("qlist" "line-item"))
					     (:file "provider" :depends-on ("qlist"))
					     (:file "geography")
					     (:file "tags")
					     (:file "configuration")
					     (:file "cart" :depends-on ("qlist" "line-item"))))
			       (:file "images"
				      :depends-on (models))
			       (:file "widgets"
				      :depends-on ("images"))
			       (:file "layout"
				      :depends-on ("views" models "widgets"))
			       (:file "views"
				      :depends-on (models "parser")))
		  :depends-on ("souk" conf))))

