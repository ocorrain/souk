(in-package #:souk)


;; (defview cart-checkout-view
;;     (:type table :inherit-from '(:scaffold shopping-cart))
;;   (cookie :hidep t)
;;   (customer :hidep t)
;;   (last-active :hidep t))

(defclass qlist-presentation (form-presentation)
  ())

(defun generate-fields-from-qlist (qlist)
  (mapcar (lambda (item)
	    (make-instance 'form-view-field
			   :slot-name (symbolicate (sku (qlist-entry-item item)))
			   :reader item
			   :label (title (qlist-entry-item item))
			   :writer (lambda (new-value obj)
				     (set-item-quantity item obj new-value)
				     (with-html
				       (:pre
					(describe item *weblocks-output-stream*))))
			   :parse-as (make-instance 'integer-parser :min 0 :max 100)
			   :present-as (make-instance 'qlist-presentation)))
	  (items qlist)))

(defun make-cart-view (cart)
  (make-instance 'form-view

		 :persistp nil
		 :fields (generate-fields-from-qlist cart)))

(defmethod render-view-field ((field form-view-field) (view form-view)
                              widget (presentation qlist-presentation) value obj
                              &rest args &key validation-errors &allow-other-keys)
  (let* ((attribute-slot-name (attributize-name (view-field-slot-name field)))
         (validation-error (assoc attribute-slot-name validation-errors
                                  :test #'string-equal
                                  :key #'view-field-slot-name))
         (field-class (concatenate 'string attribute-slot-name
                                   (when validation-error " item-not-validated"))))
    (with-html
      (:li :class field-class
           (:span :class "label"
                  (:span :class "slot-name"
                         (:span :class "extra"
                                (str (view-field-label field)) ":&nbsp;"
                                (when (form-view-field-required-p field)
                                  (htm (:em :class "required-slot" "(required)&nbsp;"))))))
;	   (:b (str (title (qlist-entry-item (view-field-reader field)))))
	   (:b " - ")
           (apply #'render-view-field-value
                  value presentation
                  field view widget obj
                  args)
	   (:b " + ")
           (when validation-error
             (htm (:p :class "validation-error"
                      (:em
                       (:span :class "validation-error-heading" "Error:&nbsp;")
                       (str (format nil "~A" (cdr validation-error)))))))))))

(defmethod render-view-field-value (value (presentation qlist-presentation)
				    (field form-view-field) (view form-view) widget obj
				    &rest args &key intermediate-values field-info &allow-other-keys)
  (declare (special *presentation-dom-id*))
  (let ((attributized-slot-name (if field-info
                                  (attributize-view-field-name field-info)
                                  (attributize-name (view-field-slot-name field)))))
    (multiple-value-bind (intermediate-value intermediate-value-p)
	(form-field-intermediate-value field intermediate-values)
      (with-html
	(:input :type "text" :name attributized-slot-name
		:value (format nil "~D"
			       (qlist-entry-quantity (if intermediate-value-p
							 intermediate-value
							 (view-field-reader field))))
		  :maxlength 3
                  :size 3
		  :id *presentation-dom-id*)))))

(defmethod print-view-field-value (value (presentation qlist-presentation)
				   field view widget obj &rest args)
;  (declare (ignore presentation obj view field args))
  "Bank" ;; (format nil "~D" (qlist-entry-quantity (view-field-reader field)))
  )

;; (defclass qlist-entry-parser (parser)
;;   ())

;; (defmethod parse-view-field-value ((parser qlist-entry-parser) value obj
;; 				   (view form-view) (field form-view-field) &rest args)
;;   (let ((data (view-field-reader field)))))
