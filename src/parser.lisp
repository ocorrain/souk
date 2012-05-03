(in-package #:souk)

(defclass object-ids-parser (checkboxes-parser)
  ((class-name :type (or symbol null)
	       :initform nil
	       :initarg :class-name
	       :accessor object-ids-parser-class-name
	       :documentation "Class of the objects whose ids are being parsed")
   (test :type function
	 :initform (constantly t)
	 :initarg :test
	 :accessor object-ids-parser-test
	 :documentation "A function of one argument that determines
         whether the parsed object is valid."))
   (:default-initargs :error-message "Zowee!")
   (:documentation "A parser designed to convert a list of object ids - integers - into a list of object instances"))

(defmethod parse-view-field-value :around ((parser object-ids-parser) value obj
				   (view form-view) (field form-view-field)
					   &rest args)
  (declare (ignore args))
  (multiple-value-bind (arg1 result result2)
      (call-next-method)
    (declare (ignore result2))
    (when (listp result)
      (let ((list-of-objects
	     (mapcar (lambda (object-id)
		       (find-persistent-object-by-id
			(class-store (object-ids-parser-class-name parser))
			(object-ids-parser-class-name parser)
			(parse-integer (symbol-name object-id))))
		     result)))
	(values arg1 list-of-objects list-of-objects)))))

