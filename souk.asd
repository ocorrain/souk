;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:souk-asd
  (:use :cl :asdf))

(in-package :souk-asd)

(defsystem souk
    :name "souk"
    :version "0.0.1"
    :maintainer ""
    :author "Tiarnan O'Corrain"
    :licence ""
    :description "souk"
    :depends-on (:weblocks-elephant :cl-who :lisp-magick :cl-fad :cl-html-parse :drakma :alexandria)
    :components ((:file "souk")
		 (:module conf
		  :components ((:file "stores"))
		  :depends-on ("souk"))
		 (:module src
		  :components ((:file "utils")
			       (:file "acceptor")
			       (:file "init-session")
			       (:file "parser")
			       (:module presentations
				:components ((:file "funcall-presentation")
					     (:file "short-list-presentation")))
			       (:module models
				:components ((:file "qlist")
					     (:file "customer")
					     (:file "line-item")
					     (:file "single-item"
						    :depends-on ("line-item"))
					     (:file "bundle"
						    :depends-on ("qlist" "line-item"))
					     (:file "provider"
						    :depends-on ("qlist"))
					     (:file "geography")
					     (:file "tags")
					     (:file "configuration")
					     (:file "cart"
						    :depends-on ("qlist" "line-item"))))
			       (:module views
				:components ((:file "single-item-views")
					     (:file "bundle-views")
					     (:file "postage-views")
					     (:file "configuration-views")
					     (:file "tag-views")
					     (:file "customer-views")
					     (:file "cart-views"))
				:depends-on (models presentations "parser"))
			       (:module widgets
				:components ((:file "cart-widgets"
						    :depends-on ("qlist-widget"))
					     (:file "stack")
					     (:file "image-widgets")
					     (:file "item-widget")
					     (:file "tag-widgets" :depends-on ("tag-cloud"))
					     (:file "qlist-widget")
					     (:file "geography-widgets" :depends-on ("tag-cloud"))
					     (:file "tag-cloud")
					     (:file "druid"))
				:depends-on ("images" presentations))
			       (:file "images"
				      :depends-on (models))
			       (:file "layout"
				      :depends-on (views models widgets)))
		  
		  :depends-on ("souk" conf))
		 (:file "test" :depends-on (src))))

