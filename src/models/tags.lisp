(in-package :souk)

(defpclass tag ()
  ((name :initarg :name :accessor tag-name :initform nil :type string :index t)
   (appears-in-menu :initarg :appears-in-menu :accessor appears-in-menu :type boolean :index t :initform t)
   (featured :initarg :featured :accessor featured :type boolean :index t :initform nil)))

(defun find-tagged-items-by-class (tag class)
  (let ((items (find-persistent-objects (class-store class) class)))
    (remove-if-not (lambda (item)
		     (member tag (tags item))) items)))

(defun find-tag-by-name (tag-name)
  (find tag-name (find-persistent-objects (class-store 'tag) 'tag)
	:key #'tag-name :test #'equal))

(defun all-tags ()
  (find-persistent-objects (class-store 'tag) 'tag))

(defun all-menu-tags ()
  (find-persistent-objects (class-store 'tag) 'tag
			   :filter-fn (lambda (tag)
					(not (appears-in-menu tag)))))

(defun all-tag-names ()
  (mapcar #'tag-name (all-tags)))

(defun get-tagged-single-objects (tag-widget sort range &rest args &key countp)
  (declare (ignore args))
  (let ((objects (find-persistent-objects (class-store 'single-item) 'single-item
			   :order-by sort
			   :range range
			   :filter-fn (lambda (item)
					(not (member (tag tag-widget) (tags item)))))))
    (if countp
	(length objects)
	objects)))

(defun tag-item (tag line-item)
  (setf (tags line-item) (adjoin tag (tags line-item) :test #'equalp)))


(defun untag-item (tag line-item)
  (setf (tags line-item)
	(remove tag (tags line-item) :test #'equalp)))

