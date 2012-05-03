(in-package #:souk)


(defwidget add-image-widget ()
  ((line-item :accessor line-item :initform nil :initarg :line-item))
  (:documentation "Class will present a simple HTML form to upload a
  new image, resize it, and add it to the image catalogue for the data-object"))

(defmethod render-widget-body ((obj add-image-widget) &rest args)
  (declare (ignore args))
  (with-slots (line-item) obj
    (with-html-form (:post (make-action (lambda (&key upload action)
					  (declare (ignore action))
					  (destructuring-bind (path filename content-type)
					      upload
					    (add-image path filename line-item))))
			   :enctype "multipart/form-data" :use-ajax-p nil)
      (:input :type "file" :name "upload")
      (:input :type "submit" :value "Upload"))))

(defwidget display-images-widget ()
  ((line-item :accessor line-item :initform nil :initarg :line-item))
  (:documentation "Display the images associated with the line item in
  a simple gallery.  This should be extended to allow extra
  functionality (e.g. deleting images)"))

(defmethod render-widget-body ((obj display-images-widget) &rest args)
  (declare (ignore args))
  (with-slots (line-item) obj
    (display-images line-item)))
