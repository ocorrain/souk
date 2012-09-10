(in-package :souk)










(defview souk-configuration-form-view
    (:type form :inherit-from '(:scaffold souk-configuration))
  (sku-counter :hidep t)
  (order-counter :hidep t))

(defview souk-configuration-table-view
    (:type table :inherit-from '(:scaffold souk-configuration))
  (sku-counter :hidep t)
  (order-counter :hidep t))





;; (defmethod render-view-field-value (item (presentation item-presentation)
;; 				    field view widget obj &rest args &key (highlight nil)
;; 				    &allow-other-keys)
;;   (when obj (render-widget (make-table-item-widget item))))


;; (defun item-render (item)
;;   (break)
;;   (with-html "Rendering this item"))
;; :suffix-fn (lambda (view field obj &rest args)
;; 			    (declare (ignore args))
;; 			    )
;; 			    ;; 		    (break))
	       