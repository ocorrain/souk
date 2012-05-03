(in-package #:souk)

(defpclass shopping-cart (quantity-list)
  ((cookie :initarg :cookie :initform nil :accessor cookie :index t
	   :documentation "The cookie id corresponding to this shopping cart")
   (last-active :initform (get-universal-time) :accessor last-active
		:documentation "The last time this cart was accessed")
   (customer :initarg :customer :initform nil :accessor customer
	     :documentation "Object holding customer details for this cart")))

(defmethod add-item :after ((item line-item) (cart shopping-cart) quantity)
  "Update the last-active field if something is added"
  (break)
  (setf (last-active cart) (get-universal-time)))

(defmethod remove-item :after ((item line-item) (cart shopping-cart))
    "Update the last-active field if something is removed"
  (setf (last-active cart) (get-universal-time)))

(defmethod empty-qlist :after ((cart shopping-cart))
  "Update the last-active field if the cart is emptied"
  (setf (last-active cart) (get-universal-time)))

(defun get-or-initialize-cart ()
  (if-let (cart (webapp-session-value :cart))
    cart
    (let ((cart (make-instance 'shopping-cart)))
      (setf (webapp-session-value :cart) cart)
      cart)))