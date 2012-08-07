(in-package #:souk)

(defpclass provider (quantity-list)
  ((name :initarg :name :accessor name :type string :initform "")
   (geographies :accessor geographies :type list :initform nil)))

(defmethod add-postage (weight-limit price (provider provider))
  (add-item price provider weight-limit))

(defmethod get-postage-rates ((provider provider))
  (sort (copy-list (items provider)) #'< :key #'qlist-entry-quantity))

(defun geo-reader (provider)
  (when (geography provider)
    (object-id (geography provider))))

(defun geo-printer (provider)
  (when (geography provider)
    (geo-name (geography provider))))

