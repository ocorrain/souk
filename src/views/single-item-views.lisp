(in-package #:souk)

(defview single-items-view (:type table :inherit-from '(:scaffold single-item))
  (id :hidep t)
  (tags :hidep t)
  (meta :hidep t)
  (short-description :hidep t)
  (long-description :hidep t)
  (images :hidep t)
  (stock-counter-started-at :hidep t)
  (image-counter :hidep t)
  (geographies :reader #'print-geographies))

(defview single-item-data-view
    (:type data :inherit-from '(:scaffold single-item)))

(defview single-item-form-view
    (:type form :inherit-from '(:scaffold single-item))
  (id :hidep t)
  (sku :hidep t)
  (meta :hidep t)
  (images :hidep t)
  (image-counter :hidep t)
  (tags :hidep t)
  (short-description :present-as (textarea :rows 1)
		     :requiredp t)
  (long-description :present-as (textarea :rows 5)
		    :requiredp t)
  (stock-counter-started-at :hidep t)
  (stock-counter-current-value :hidep t)
;  (geographies :hidep t)
  (geographies :reader (lambda (item)
  			 (mapcar (alexandria:compose #'as-string #'object-id)
  				 (geographies item)))
  	       :present-as (checkboxes :choices #'all-geographies
  				       :label-key #'geo-name)
  	       :parse-as (object-ids :class-name 'geography)))



(defview single-item-display-view
    (:type table)
  (title :label ""
	 :present-as (funcall :function (lambda (obj)
					  (render-widget (make-table-item-widget obj))))
	 :reader (lambda (obj) obj))
  (image :reader (lambda (obj)
		   (when-let (images (images obj))
		     (get-thumb-url (random-elt images))))
	 :label ""
	 :present-as (image)
	 :slot-name nil))