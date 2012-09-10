(in-package #:souk)

(defclass short-list-presentation (text-presentation)
  ((display-length :accessor display-length :initarg :display-length :initform 10)))

(defmethod render-view-field-value (value (presentation short-list-presentation)
                                    field view widget obj &rest args)
  (let* ((val (length value))
	 (pres (display-length presentation))
	 (display-values (if (<= val pres)
			     value
			     (subseq value 0 pres))))
    (with-html
      (fmt "~{~A~^, ~}" display-values)
      (if (> val pres)
	  (htm (:i (fmt "... ~D more." (- val pres))))))))

(defmethod print-view-field-value ((value standard-object) (p funcall-presentation) field view widget obj &rest args) 
  (declare (ignore obj view field args))
  (format nil " ~A" (funcall (get-function p) value)))

(defclass item-presentation (text-presentation)
  ((item :initform nil :initarg :item :accessor item-presentation-item)
   (mode :initform :inline :initarg :mode :accessor item-presentation-mode)))