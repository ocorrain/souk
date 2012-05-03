(in-package :souk)

(defun as-string (thing)
  (format nil "~A" thing))

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
  (geographies :hidep t)
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
  (items :present-as (checkboxes :choices #'get-postage-rates))
  (geography :reader #'geo-reader
	     :present-as (dropdown :choices #'all-geographies
				   :label-key #'geo-name)
	     :parse-as (object-id :class-name 'geography)))

(defview geography-view (:type table :inherit-from '(:scaffold geography))
  (providers :hidep t))

(defview geography-data-view (:type data :inherit-from '(:scaffold geography)))
(defview geography-form-view (:type form :inherit-from '(:scaffold geography))
  (providers :hidep t))

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
	 :present-as (item)
	 :reader (lambda (obj) obj))
  (image :reader (lambda (obj)
		   (when-let (images (images obj))
		     (get-thumb-url (random-elt images))))
	 :label ""
	 :present-as (image)
	 :slot-name nil))


(defmethod render-view-field-value (item (presentation item-presentation)
				    field view widget obj &rest args &key (highlight nil)
				    &allow-other-keys)
  (when obj (render-widget (make-table-item-widget item))))


;; (defun item-render (item)
;;   (break)
;;   (with-html "Rendering this item"))
;; :suffix-fn (lambda (view field obj &rest args)
;; 			    (declare (ignore args))
;; 			    )
;; 			    ;; 		    (break))
	       