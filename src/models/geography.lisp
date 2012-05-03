(in-package #:souk)

(defpclass geography ()
  ((geography-name :initarg :name :accessor geo-name)
   (providers :initform nil :accessor geo-providers)))

(defun all-geographies (&rest args)
  (declare (ignore args))
  (find-persistent-objects (class-store 'geography) 'geography))


(defun print-geographies (item)
  (when (geographies item)
    (format nil "~{~A~^, ~}" (mapcar #'geo-name (geographies item)))))
