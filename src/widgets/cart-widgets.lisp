(in-package #:souk)

(defwidget mini-shopping-cart-widget ()
  ((item :accessor item :initform nil :initarg :item)
   (cart :accessor shopping-cart :initform nil :initarg :shopping-cart)))

(defmethod render-widget-body ((obj mini-shopping-cart-widget) &rest args)
  (declare (ignore args))
  (with-slots (item cart) obj
    (with-html-form (:post (make-action (lambda (&key qty action &allow-other-keys)
					  (declare (ignore action))
					  (let ((quantity (ignore-errors (parse-integer qty))))
					    (when quantity (add-item item (shopping-cart item)
								     quantity))))))
      ((:select :name "qty")
       ((:option :value 0 :selected "selected") (str 0))
       (dotimes (i 49)
	 (htm ((:option :value (+ i 1))
	       (str (+ i 1))))))
					; (:input :type "text" :name "qty")
      (:input :type "submit" :value "Add to cart"))))

(defwidget shopping-cart-widget ()
  ((cart :accessor shopping-cart :initform nil :initarg :shopping-cart)
   (mode :accessor display-mode :initform :full :initarg :display-mode)))

(defmethod render-widget-body ((obj shopping-cart-widget) &rest args)
  (declare (ignore args))
  (with-slots (cart mode) obj
    (case mode
      (:full
       (with-html
	 (if (empty? cart)
	     (htm (str "The shopping cart is empty"))
	     (htm (:table (:thead
			   (:tr (:th "Quantity") (:th "Item price")
				(:th "Subtotal") (:th "Item")))
			  (:tfoot
			   (:tr (:th " ")
				(:th (fmt "Total weight: ~Ag" (get-weight cart)))
				(:th (str (print-price (get-price cart))))
				(:th "TOTAL")))
			  (:tbody
			   (dolist (i (items cart))
			     (let* ((quantity (qlist-entry-quantity i))
				    (item (qlist-entry-item i))
				    (price (get-price item)))
			       (htm (:tr (:td (str quantity))
					 (:td (str (print-price price)))
					 (:td (str (print-price (* price quantity))))
					 (:td (str (title item))))))))))))))))