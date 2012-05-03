(in-package #:souk)

(defun print-price (price-in-cents)
  (multiple-value-bind (euro cent)
      (floor price-in-cents 100)
    (format nil "€~:D.~2,'0D" euro cent)))

(defwidget item-widget ()
  ((item :accessor item :initform nil :initarg :item)))

(defmethod render-widget-body ((obj item-widget) &rest args)
  (declare (ignore args))
  (with-slots (item) obj
    (with-html (:h1 (esc (title item)))
	       (:p (:i (str (print-price (get-price item)))))
	       (:p (esc (short-description item))))))