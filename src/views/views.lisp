(in-package :souk)


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



(defview bundle-view (:type table :inherit-from '(:scaffold bundle))
  (id :hidep t)
  (tags :hidep t)
  (meta :hidep t)
  (long-description :hidep t)
  (images :hidep t)
  (stock-counter-started-at :hidep t)
  (items :hidep t)
  (image-counter :hidep t)
  (geographies :reader #'print-geographies))

(defview bundle-items-view
    (:type table :inherit-from '(:scaffold single-item))
  (id :hidep t)
  (tags :hidep t)
  (meta :hidep t)
  (short-description :hidep t)
  (featured :hidep t)
  (published :hidep t)
  (long-description :hidep t)
  (stock-counter-started-at :hidep t)
  (image-counter :hidep t)
  (images :hidep t)
  (stock/week :hidep t)
  (geographies :reader #'print-geographies)
  (stock-counter-current-value :hidep t))

(defview bundle-data-view
    (:type data :inherit-from '(:scaffold bundle)))

(defview bundle-form-view
    (:type form :inherit-from '(:scaffold bundle))
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
  (geographies :hidep t)
  (items :hidep t)
  (geographies :reader (lambda (item)
  			 (mapcar (alexandria:compose #'as-string #'object-id)
  				 (geographies item)))
  	       :present-as (checkboxes :choices #'all-geographies
  				       :label-key #'geo-name)
  	       :parse-as (object-ids :class-name 'geography)))



(defview provider-view (:type table :inherit-from '(:scaffold provider))
  (items :hidep t)
  (geography :reader #'geo-printer))

(defview provider-data-view (:type data :inherit-from '(:scaffold provider)))

(defview provider-form-view (:type form :inherit-from '(:scaffold provider))
  (items :hidep t;; :present-as (checkboxes :choices #'get-postage-rates)
	 )
  (geography :reader #'geo-reader
	     :present-as (dropdown :choices #'all-geographies
				   :label-key #'geo-name)
	     :parse-as (object-id :class-name 'geography)))

(defview geography-view (:type table :inherit-from '(:scaffold geography))
  (countries :present-as (short-list :format-string "~{~A~^, ~}")))


(defview geography-data-view (:type data :inherit-from '(:scaffold geography)))

(defview geography-form-view
    (:type form :inherit-from '(:scaffold geography))
  (countries :present-as w3-country :writer #'add-selection-to-geography))

(defview tags-view (:type table :inherit-from '(:scaffold tag)))

(defview tags-data-view (:type data :inherit-from '(:scaffold tag)))

(defview tags-form-view (:type form :inherit-from '(:scaffold tag)))

(defview souk-configuration-form-view
    (:type form :inherit-from '(:scaffold souk-configuration))
  (sku-counter :hidep t)
  (order-counter :hidep t))

(defview souk-configuration-table-view
    (:type table :inherit-from '(:scaffold souk-configuration))
  (sku-counter :hidep t)
  (order-counter :hidep t))



(defclass item-presentation (text-presentation)
  ((item :initform nil :initarg :item :accessor item-presentation-item)
   (mode :initform :inline :initarg :mode :accessor item-presentation-mode)))


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

