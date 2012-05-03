(in-package #:souk)

(defpclass souk-configuration ()
  ((shop-name :initarg :shop-name :initform "" :accessor shop-name :type string)
   (shop-description :initarg :shop-description :initform "" :accessor shop-description :type string)
   (thumbnail-width :initarg :thumbnail-width :initform 200 :accessor thumbnail-width
		    :type (integer 50 400))
   (thumbnail-height :initarg :thumbnail-height :initform 200 :accessor thumbnail-height
		    :type (integer 50 400))
   (display-width :initarg :display-width :initform 500 :accessor display-width
		    :type (integer 200 1000))
   (display-height :initarg :display-height :initform 500 :accessor display-height
		    :type (integer 200 1000))
   (sku-prefix :initarg :sku-prefix :initform "SKU" :accessor sku-prefix :type (string 3))
   (sku-counter :initform 0 :accessor sku-counter :type integer)
   (order-prefix :initarg :order-prefix :initform "ORD" :accessor order-prefix :type (string 3))
   (order-counter :initform 0 :accessor order-counter :type integer))
  (:documentation "Singleton object designed to hold configuration
  items for an instance of souk"))

(defun get-next-sku ()
  (prog1
      (format nil "~A~7,'0d"
	      (sku-prefix *souk-configuration*)
	      (sku-counter *souk-configuration*))
    (incf (sku-counter *souk-configuration*))))