(in-package #:souk)

(defpclass shopping-cart (quantity-list)
  ((cookie :initarg :cookie :initform (hunchentoot::create-random-string 20 20)
	   :accessor cookie :index t
	   :documentation "The cookie id corresponding to this shopping cart")
   (last-active :initform (get-universal-time) :accessor last-active
		:documentation "The last time this cart was accessed")
   (customer :initarg :customer :initform nil :accessor customer
	     :documentation "Object holding customer details for this cart")))

(defmethod add-item :after ((item line-item) (cart shopping-cart) quantity)
  "Update the last-active field if something is added"
  (setf (last-active cart) (get-universal-time)))

(defmethod remove-item :after ((item line-item) (cart shopping-cart))
    "Update the last-active field if something is removed"
  (setf (last-active cart) (get-universal-time)))

(defmethod empty-qlist :after ((cart shopping-cart))
  "Update the last-active field if the cart is emptied"
  (setf (last-active cart) (get-universal-time)))

(defun get-or-initialize-cart ()
  (let ((cart-var (webapp-session-value "cart")))
    (if (and cart-var (typep cart-var 'shopping-cart))
	cart-var
	(let ((cart-cookie (cookie-in "soukart")))
	  (if cart-cookie
	      (let ((cart (find-cart-by-cookie cart-cookie)))
		(if cart
		    cart
		    (initialize-cart)))
	      (initialize-cart))))))

(defun initialize-cart ()
  (let ((cart (make-instance 'shopping-cart)))
    (set-cookie "soukart" :value (cookie cart))
    (setf (webapp-session-value "cart") cart)
    cart))

;; (defun get-or-initialize-cart ()
;;   (let ((cart-cookie (cookie-in "soukart")))
;;     (if cart-cookie
;; 	(let ((cart (find-cart-by-cookie cart-cookie)))
;; 	  (if cart
;; 	      cart
;; 	      (let ((cart (make-instance 'shopping-cart)))
;; 		(set-cookie "soukart" :value (cookie cart))
;; 		(setf (webapp-session-value "cart") cart)
;; 		cart))))))


(defun all-carts ()
  (find-persistent-objects (class-store 'shopping-cart) 'shopping-cart))

(defun find-cart-by-cookie (cookie)
  (ele:get-instance-by-value 'shopping-cart 'cookie cookie))
