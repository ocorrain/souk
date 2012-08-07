
(defpackage #:souk
  (:use :cl :weblocks
        :f-underscore :anaphora :cl-who :alexandria :weblocks-elephant)
  (:import-from :hunchentoot #:header-in
		#:set-cookie #:set-cookie* #:cookie-in
		#:user-agent #:referer)
  (:documentation
   "A web application based on Weblocks."))

(in-package :souk)

(export '(start-souk stop-souk))

;; A macro that generates a class or this webapp
(defun get-dependencies (dependency-type &rest paths)
  (mapcar (lambda (path)
	    (make-instance dependency-type
			   :local-path (merge-pathnames (asdf:system-source-directory :souk) path)
			   :url (puri:uri (merge-pathnames path #p"/")))) paths))


(defun get-blueprint-dependencies ()
  (get-dependencies 'stylesheet-dependency
		    #p"pub/stylesheets/blueprint/screen.css"
		    #p"pub/stylesheets/blueprint/plugins/tabs/screen.css"))

(defun get-js-dependencies ()
  (get-dependencies 'script-dependency #p"pub/scripts/livepipe.js" #p"pub/scripts/tabs.js"))

(defwebapp souk
    :prefix "/"
    :description "souk: a bazaar application"
    :init-user-session 'souk::init-user-session
    :autostart nil                   ;; have to start the app manually
    :dependencies (append (get-js-dependencies)
			  (get-dependencies 'stylesheet-dependency #p"pub/stylesheets/souk.css"))
    :ignore-default-dependencies nil ;; accept the defaults
    :debug t)

(defwebapp souk-admin
    :prefix "/admin"
    :description "souk-admin: administering souk"
    :init-user-session 'souk::init-admin-session
    :autostart nil                   ;; have to start the app manually
    :dependencies (append (get-js-dependencies)
			  (get-dependencies 'stylesheet-dependency #p"pub/stylesheets/souk.css"))
    :ignore-default-dependencies nil ;; accept the defaults
    :debug t)


;; Top level start & stop scripts
(defvar *souk-configuration* nil
  "This special variable holds the singleton configuration instance for the entire store")

(defun get-souk-configuration ()
  "This grabs the singleton instance from the store, and sets
  *souk-configuration*.  If no instance of the class is found, creates
  one and sets *souk-configuration*"
  (let* ((configuration-items (find-persistent-objects (class-store 'souk-configuration) 'souk-configuration))
	 (length (length configuration-items)))
    (cond ((> length 1) (error "~A configuration objects were found in the database.  Fatal error." length))
	  ((zerop length) (setf *souk-configuration* (make-instance 'souk-configuration)))
	  (t (setf *souk-configuration* (first configuration-items))
	     (length (find-persistent-objects (class-store 'single-item) 'single-item))))))

(defun get-config-option (option)
  (funcall option *souk-configuration*))

(defun start-souk (&rest args)
  "Starts the application by calling 'start-weblocks' with appropriate
arguments."
  (apply #'start-weblocks args)
  (start-webapp 'souk)
  (start-webapp 'souk-admin)
  (get-souk-configuration))

(defun stop-souk ()
  "Stops the application by calling 'stop-weblocks'."
  (stop-webapp 'souk)
  (stop-webapp 'souk-admin)
  (stop-weblocks))

