(in-package #:souk)

;; (defwidget country-group-selector ()
;;   ((geography :initarg :geography :initform nil :accessor geography)))

;; (defmethod render-widget-body ((obj country-group-selector) &rest args)
;;   (declare (ignore args))
;;   (with-slots (geography) obj
;;     (flet ((selection-action (geo)
;; 	     (lambda (&rest args)
;; 	       (declare (ignore args))
;; 	       (when (get-selection geo)
;; 		 (if (intersection (get-selection geo) (countries geography)
;; 				   :test #'string-equal)
;; 		     (remove-selection-from-geography geo geography)
;; 		     (add-selection-to-geography geo geography))
		 
;; 		 (mark-dirty obj)))))
      
;;       (with-html
;; 	(dolist (g (get-country-group-list))
;; 	  (render-link (make-action (selection-action g))
;; 		       g :class "country-group" :title g)
;; 	  (htm " - "))
;; 	(:hr)
;; 	(dolist (c (get-country-list))
;; ;	  (if (country))
;; 	  (render-link (make-action (selection-action c))
;; 		       c :class "country" :title c
;; 		       :render-fn (lambda (title)
;; 				    (if (country-in c geography)
;; 					(with-html (:b (str title)))
;; 					(with-html (str title)))))
;; 	  (htm " - "))))))

(defun geography-selector (geography)
  (make-instance 'tag-cloud
		 :tag-object geography
		 :all-tags #'w3-selection-names
		 :accessor #'countries
		 :test #'string-equal
		 :remove-fn #'remove-selection-from-geography
		 :add-fn #'add-selection-to-geography
		 :render-fn (lambda (geo)
			      (with-html (str geo)))))

