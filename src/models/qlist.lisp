(in-package #:souk)

(defclass quantity-list ()
  ((items :initarg :items :initform '() :accessor items
	  :documentation "An alist of the form (ITEM or BUNDLE . QUANTITY)")))

;; (defstruct (qlist-entry (:type list)) item quantity)

(defclass qlist-entry ()
  ((item :initarg :item :accessor qlist-entry-item :initform '())
   (quantity :initarg :quantity :accessor qlist-entry-quantity :initform 0)))

(defun make-qlist-entry (&key item quantity)
  (make-instance 'qlist-entry :item item :quantity quantity))

(defmethod add-item (item (qlist quantity-list) quantity)
  (set-item-quantity item qlist (+ (get-item-quantity item qlist) quantity)))

(defmethod subtract-items (item (qlist quantity-list) quantity)
  (let ((current-quantity (get-item-quantity item qlist)))
    (if (>  current-quantity quantity)
	(set-item-quantity item qlist (- current-quantity quantity))
	(remove-item item qlist))))


(defmethod set-item-quantity (item (qlist quantity-list) quantity)
  (with-slots (items) qlist
    (if-let (entry (find item items :key #'qlist-entry-item))
      (setf (qlist-entry-quantity entry) quantity)
      (push (make-qlist-entry :item item :quantity quantity) items))))

(defmethod get-item-quantity (item (qlist quantity-list))
  (if-let (found (find item (items qlist) :key #'qlist-entry-item))
    (qlist-entry-quantity found)
    0))

(defmethod remove-item (item (qlist quantity-list))
  (setf (items qlist) (remove item (items qlist) :key #'qlist-entry-item)))

(defmethod empty-qlist ((qlist quantity-list))
  (setf (items qlist) nil))

(defmethod empty? ((qlist quantity-list))
  (null (items qlist)))

(defmethod qlist-reduce ((qlist quantity-list)
			 &key (reduce #'+) (item-function #'identity) (combine #'*))
  (reduce reduce (mapcar (lambda (qentry)
			   (funcall combine
				    (funcall item-function (qlist-entry-item qentry))
				    (qlist-entry-quantity qentry)))
			 (items qlist))))

(defmethod get-price ((qlist quantity-list))
  (qlist-reduce qlist :item-function #'get-price))

(defmethod get-weight ((qlist quantity-list))
  (qlist-reduce qlist :item-function #'get-weight))

(defmethod total-number-of-items ((qlist quantity-list))
  (qlist-reduce qlist :item-function (constantly 1)))