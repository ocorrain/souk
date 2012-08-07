
(in-package :souk)

;; Define callback function to initialize new sessions
(defun init-user-session (root)
  (setf (widget-children root)
	(list (generic-header-widget)
	      (make-navigation 'root
			       (list "Shop" (make-tags-navigation) "shop")
			       (list "Checkout" (make-checkout-widget) "checkout")))))


(defun init-admin-session (root)
  (setf (widget-children root)
	(list (generic-header-widget)
	      (make-admin-navigation))))

(defun generic-header-widget ()
  (lambda (&rest args)
    (declare (ignore args))
    (with-html
      (:img :src "/pub/images/widget/header/new-logo.jpg"))))