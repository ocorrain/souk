(in-package #:souk)

(defclass funcall-presentation (text-presentation)
  ((function :accessor get-function :initarg :function)))

(defmethod render-view-field-value (value (presentation funcall-presentation)
                                    field view widget obj &rest args
                                    &key highlight &allow-other-keys)
  (declare (ignore args highlight))
  (if (null value)
      (call-next-method)
      (with-html
        (:span :class "text"
	       (str (funcall (get-function presentation)
			     value))))))


(defmethod print-view-field-value ((value standard-object) (p funcall-presentation) field view widget obj &rest args) 
  (declare (ignore obj view field args))
  (format nil " ~A" (funcall (get-function p) value)))

