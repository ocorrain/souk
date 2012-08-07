(in-package #:souk)

;; (defview mini-shopping-cart-view (:type form :persistp nil)
;;   (quantity :requiredp t))

;; (defun add-items-to-cart-form ()
;;   (make-simpleform 'mini-shopping-cart-view
;; 		   :submit-label "Add to cart"
;; 		   :on-success (lambda (widget)
;; 				 t)
;; 				 ;; (add-item (item widget) (cart widget) )
;; 				 ))

(defwidget mini-shopping-cart-widget ()
  ((item :accessor item :initform nil :initarg :item)))

(defmethod render-widget-body ((obj mini-shopping-cart-widget) &rest args)
  ;; FIXME
  (declare (ignore args))
  (let ((cart (get-or-initialize-cart)))
    (with-slots (item) obj
      (render-link (make-action (lambda (&rest args)
				  (declare (ignore args))
				  (add-item item
					    cart
					    1)
				  (mark-dirty (get-widget-by-id "cart"))))
		   "Add to cart"))))


(defwidget shopping-cart-widget ()
  ((mode :accessor display-mode :initform :full :initarg :display-mode)))

(defmethod render-widget-body ((obj shopping-cart-widget) &rest args)
  (declare (ignore args))
  (render-shopping-cart obj (display-mode obj)))


(defgeneric render-shopping-cart (widget mode))

(defmethod render-shopping-cart ((obj shopping-cart-widget) (mode (eql :short)))
  (with-cart
    (with-html (:b (fmt "~D items in cart. " (total-number-of-items cart)))
	       (render-link (make-action (lambda (&rest args)
					   (declare (ignore args))
					   (setf (display-mode obj) :full)))
			    "View cart")
	       (fmt " | ")
	       (render-link (make-action (lambda (&rest args)
					   (declare (ignore args))))
			    "Check out"))))

(defmacro with-cart (&body body)
  `(let ((cart (get-or-initialize-cart)))
     ,@body))

(defmethod render-shopping-cart ((obj shopping-cart-widget) (mode (eql :full)))
  (with-cart
      (with-html
	(cond ((empty? cart) (htm (str "The shopping cart is empty")))
	      (t (qlist->table cart
			       (lambda (cart)
				 (declare (ignore cart))
				 (list "Quantity" "Item price" "Subtotal" "Item"))
			       (lambda (cart)
				 (list " "
				       (format nil "Total weight: ~Ag" (get-weight cart))
				       (print-price (get-price cart))
				       "TOTAL"))
			       (lambda (i)
				 (let* ((quantity (qlist-entry-quantity i))
					(item (qlist-entry-item i))
					(price (get-price item)))
				   (list quantity
					 (print-price price)
					 (print-price (* price quantity))
					 (title item)))))
		 (render-link (make-action (lambda (&rest args)
					     (declare (ignore args))
					     (setf (display-mode obj) :short)))
			      "Collapse")
		 (fmt " | ")
		 (render-link (make-action (lambda (&rest args)
					     (declare (ignore args))
					     (setf (widget-parent obj)
						   (make-checkout-widget))
					     (mark-dirty (widget-parent obj))))
			      "Check out"))))))

(defwidget checkout-widget (shopping-cart-widget)
  ((customer :accessor customer :initarg :customer)))

(defmethod render-widget-body ((obj checkout-widget) &rest args)
  (declare (ignore args))
  (with-html
    (:h1 "Checkout!")
    (render-shopping-cart obj :full)))





(defun make-shopping-cart-widget (mode)
  (make-instance 'shopping-cart-widget
  		 :dom-id "cart"
  		 :display-mode mode))




(defun make-mini-shopping-cart (item)
  (make-instance 'mini-shopping-cart-widget
		 :item item))


(defun make-table-item-widget (item)
  (make-instance 'widget
		 :children (list (make-instance 'item-widget :item item)
				 (make-mini-shopping-cart item))))


;;;;;;;  Checking out

(defwidget checkout (wizard)
  ())

(defun make-checkout-widget ()
  ;; (make-instance 'checkout
  ;; 		 :data (list (get-or-initialize-cart)
  ;; 			     (make-instance 'customer))
  ;; 		 :on-complete 'checkout-complete-function)
  (make-quickform (make-cart-view (get-or-initialize-cart)))
  )


(defmethod wizard-form-view ((wizard checkout) (data shopping-cart) (step (eql 1)))
  (make-cart-view data))

(defmethod wizard-form-view ((wizard checkout) (data customer) (step (eql 2)))
  nil)

(defmethod wizard-render-summary-page ((wizard checkout))
  (call-next-method))

(defun checkout-complete-function (wizard)
  (destructuring-bind (cart customer)
      (wizard-data wizard)
    ;; ordering code goes here
    nil))
