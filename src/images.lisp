(in-package #:souk)

(defparameter *image-path*
  (merge-pathnames (make-pathname :directory '(:relative "data" "images"))
		   (asdf-system-directory :souk)))

(defparameter *thumbs-path*
  (merge-pathnames #p"pub/images/thumbs/"
		   (asdf-system-directory :souk)))

(defparameter *display-path*
  (merge-pathnames #p"pub/images/display/"
		   (asdf-system-directory :souk)))

(defun pathname-name-concat (path concat-string)
  (make-pathname :directory (pathname-directory path)
		 :name (concatenate 'string (pathname-name path) concat-string)
		 :type (pathname-type path)))

(defun merge-name-and-type-with (path defaults)
  (merge-pathnames (make-pathname :name (pathname-name path) :type (pathname-type path))
		   defaults))

(defun get-thumb-path (path)
  (merge-name-and-type-with path *thumbs-path*))

(defun get-full-size-path (path)
  (merge-name-and-type-with path *display-path*))

(defun get-original-image-path (path)
  (merge-name-and-type-with path *image-path*))

(defparameter *thumbs-uri* #p"/pub/images/thumbs/")
(defparameter *display-uri* #p"/pub/images/display/")

(defun get-thumb-url (path)
  (namestring (merge-name-and-type-with path *thumbs-uri*)))

(defun get-full-url (path)
  (namestring (merge-name-and-type-with path *display-uri*)))

(defun create-thumbnail (filename thumbname width height)
  "Create a thumbnail the image in FILENAME with a max size of WIDTH x HEIGHT
pixel (but with the original aspect ratio) and save it in THUMBNAME."
  (if (or (pathnamep filename)
	  (pathnamep thumbname))
      (create-thumbnail (namestring filename) (namestring thumbname) width height)
      (lisp-magick:with-magick-wand (wand :load filename)
	(let ((a (/ (lisp-magick:magick-get-image-width wand)
		    (lisp-magick:magick-get-image-height wand))))
	  (if (> a (/ width height))
	      (lisp-magick:magick-scale-image wand width (truncate (/ width a)))
	      (lisp-magick:magick-scale-image wand (truncate (* a height)) height)))
	(lisp-magick:magick-write-image wand thumbname))))

(defun resize-all-images-for-class (class)
  (mapc (lambda (key item)
	  (declare (ignore key))
	  (when-let (images (images item))
	    (dolist (i images)
	      (let ((dest-path (make-pathname
				:name (pathname-name i) :type (pathname-type i)
				:defaults *image-path*)))

		(create-thumbnail dest-path (get-thumb-path dest-path)
				  (get-config-option 'thumbnail-width)
				  (get-config-option 'thumbnail-height))
		(create-thumbnail dest-path (get-full-size-path dest-path)
				  (get-config-option 'display-width)
				  (get-config-option 'display-height))))))
	(find-persistent-objects (class-store class) class)))

  ;; (ele:map-btree (lambda (key item)
  ;; 		   (declare (ignore key))
  ;; 		   (when-let (images (images item))
  ;; 		     (dolist (i images)
  ;; 		       (let ((dest-path (make-pathname
  ;; 					 :name (pathname-name i) :type (pathname-type i)
  ;; 					 :defaults (image-path *web-store*))))

  ;; 			 (create-thumbnail dest-path (get-thumb-path dest-path)
  ;; 					   (get-config-option 'thumbnail-width)
  ;; 					   (get-config-option 'thumbnail-height))
  ;; 			 (create-thumbnail dest-path (get-full-size-path dest-path)
  ;; 					   (get-config-option 'display-width)
  ;; 					   (get-config-option 'display-height))))))
  ;; 		 (items *web-store*)))

;; (defun display-gallery (images id)
;;   (lambda (stream)
;;     (let ((thumb-width (get-config-option 'thumbnail-width))
;; 	  (thumb-height (get-config-option 'thumbnail-height)))
;;       (with-html-output (s stream)
;; 	(lightbox-gallery s id)
;; 	((:div :id id)
;; 	 (:ul
;; 	  (dolist (i images)
;; 	    (htm (:li ((:a :href (get-full-url i))
;; 		       (:img :src (get-thumb-url i))))))))))))


;; (defmethod edit-display-images ((item line-item) stream)
;;   (when (images item)
;;     (let ((thumb-width (get-config-option 'thumbnail-width))
;; 	  (thumb-height (get-config-option 'thumbnail-height)))
;;       (with-html-output (s stream)
;; 	(lightbox-gallery s "gallery")
;; 	((:div :id "gallery")
;; 	 ((:form :action (get-url item) :method :post)
;; 	  (:ul
;; 	   (dolist (i (images item))
;; 	     (htm (:li ((:a :href (get-full-url i))
;; 			(:img :src (get-thumb-url i)))
;; 		       (:input :type "checkbox" :name "imgdel" :value i)))))
;; 	  (:input :type "submit" :value "Delete")))))))

(defmethod display-images ((item line-item))
  (when (images item)
    (with-html
      ((:div :id "gallery")
       (:ul
	(dolist (i (images item))
	  (htm (:li ((:a :href (get-full-url i))
		     (:img :src (get-thumb-url i)))))))))))

(defun add-image (path original-filename line-item)
  (let ((type (string-downcase (pathname-type original-filename)))
	(stub (get-next-image-stub line-item)))
    (let ((dest-path (make-pathname
		      :name stub :type type
		      :defaults *image-path*)))
      (cl-fad:copy-file path dest-path)
      (create-thumbnail dest-path (get-thumb-path dest-path)
			(get-config-option 'thumbnail-width)
			(get-config-option 'thumbnail-height))
      (create-thumbnail dest-path (get-full-size-path dest-path)
			(get-config-option 'display-width)
			(get-config-option 'display-height))
      (push (make-pathname :name stub :type type) (images line-item)))))
