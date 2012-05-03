(in-package :souk)

(defpclass tag ()
  ((name :initarg :name :accessor tag-name :initform nil :type string)))

(defun find-tagged-items-by-class (tag class)
  (let ((items (find-persistent-objects (class-store class) class)))
    (remove-if-not (lambda (item)
		     (member tag (tags item))) items)))

(defun find-tag-by-name (tag-name)
  (find tag-name (find-persistent-objects (class-store 'tag) 'tag)
	:key #'tag-name :test #'equal))
