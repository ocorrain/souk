(in-package #:souk)

(defpclass line-item ()
  ((title :initarg :title :initform ""
	  :accessor title
	  :documentation "Title or name of the line item" :type string)
   (short-description :initarg :short-description :initform ""
		      :accessor short-description
		      :documentation "A one-line description of the item"
		      :type string)
   (long-description :initarg :long-description :initform ""
		     :accessor long-description
		     :documentation "A long (paragraph length)
		     description of the item"
		     :type string)
   (packing-weight :initarg :packing-weight
		   :initform 0 :accessor packing-weight
		   :documentation "The extra weight of packaging:
		   ie. packing-weight + weight equals the total
		   shipping weight"
		   :type integer)
   (tags :initarg :tags :initform '()
	       :accessor tags
	       :documentation "A list of categories or tags into which
	       this item falls"
	       :type list)
   (sku :initarg :sku :initform (get-next-sku) :accessor sku
	:documentation "Stock-keeping unit ID"
	:type string)
   (meta :initarg :meta :initform '() :accessor meta
	 :documentation "Meta tags to be added to page for HTML
	 searchability"
	 :type list)
   (featured :initarg :featured :initform nil :accessor featured
	     :type boolean
	     :documentation "Is this to be published to the front-page
	     / featured page?")
   (published :initarg :published :initform nil
	      :accessor published
	      :documentation "Is this to be published to the site?"
	      :type boolean)
   (images :initform '() :accessor images
	   :documentation "List of images of this item"
	   :type list)
   (stock/week :initform 0 :accessor stock/week
	       :documentation "Quantity of stock available for sale per week"
	       :type integer)
   (stock-counter-started-at :initform nil :accessor stock-counter-started-at
			     :documentation "When the first unit of
			     stock was sold for this batch.
			     Stock/week will be topped up to
			     stock/week when the difference between
			     the current time and this value is
			     greater than one week"
			     :type integer)
   (stock-counter-current-value :initform 0 :accessor stock-counter-current-value
				:documentation "The current value of
				the stock counter.  This is
				incremented when an item is sold, and
				compared with the stock/week and
				stock-counter-started-at values to see
				if the sale can go through")
   (image-counter :initform 0 :accessor image-counter
		  :documentation "counter for image filenames"
		  :type number)
   (geographies :initform nil :accessor geographies
		:documentation "Geographies in which this item is available"
		:type list)))

(defmethod get-next-image-stub ((item line-item))
  (prog1
      (format nil "~A_~A" (sku item) (image-counter item))
    (incf (image-counter item))))

(defpclass single-item (line-item)
  ((weight :initarg :weight :initform 0 :accessor weight
	   :documentation "The weight of the item in grams" :type integer)
   (price :initarg :price :initform 0 :accessor price
	  :documentation "The price of the item in euro cents" :type integer)))

(defpclass bundle (line-item quantity-list)
  ((discount :initarg :discount :initform 0 :accessor discount :type integer
	     :documentation "Percentage discount for a bundle")))

(defgeneric get-price (item))

(defmethod get-price ((qlist quantity-list))
  (qlist-reduce (items qlist) :item-function #'get-price))

(defgeneric get-weight (item))

(defmethod get-weight ((qlist quantity-list))
  (qlist-reduce (items qlist) :item-function #'get-weight))

(defmethod get-price ((item single-item))
  (price item))

(defmethod get-weight ((item single-item))
  (weight item))

(defmethod get-images ((item single-item))
  (images item))

(defmethod get-images ((bundle bundle))
  (remove-duplicates
   (flatten (mapcar (compose #'images #'car) (items bundle)))))

(defmethod get-price :around ((bundle bundle))
  "Applies the bundle discount"
  (let ((initial-price (call-next-method)))
    (round (* initial-price (/ (- 100 (discount bundle)) 100)))))

(defun print-price (price-in-cents)
  (multiple-value-bind (euro cent)
      (floor price-in-cents 100)
    (format nil "€~:D.~2,'0D" euro cent)))

(defpclass provider (quantity-list)
  ((name :initarg :name :accessor name :type string :initform "")
   (geography :accessor geography :type geography :initform nil)))

(defmethod add-postage (weight-limit price (provider provider))
  (add-item price provider weight-limit))

(defmethod get-postage-rates ((provider provider))
  (sort (copy-list (items provider)) #'< :key #'qlist-entry-quantity))

(defpclass geography ()
  ((geography-name :initarg :name :accessor geo-name)
   (providers :initform nil :accessor geo-providers)))

(defun all-geographies (&rest args)
  (declare (ignore args))
  (find-persistent-objects (class-store 'geography) 'geography))

(defun geo-reader (provider)
  (when (geography provider)
    (object-id (geography provider))))

(defun geo-printer (provider)
  (when (geography provider)
    (geo-name (geography provider))))

(defun print-geographies (item)
  (when (geographies item)
    (format nil "~{~A~^, ~}" (mapcar #'geo-name (geographies item)))))

(defpclass tag ()
  ((name :initarg :name :accessor tag-name :initform nil :type string)))

(defun find-tagged-items-by-class (tag class)
  (let ((items (find-persistent-objects (class-store class) class)))
    (remove-if-not (lambda (item)
		     (member tag (tags item))) items)))

(defun find-tag-by-name (tag-name)
  (find tag-name (find-persistent-objects (class-store 'tag) 'tag)
	:key #'tag-name :test #'equal))

(defpclass souk-configuration ()
  ((shop-name :initarg :shop-name :initform "" :accessor shop-name :type string)
   (shop-description :initarg :shop-description :initform "" :accessor shop-description :type string)
   (thumbnail-width :initarg :thumbnail-width :initform 200 :accessor thumbnail-width
		    :type (integer 50 400))
   (thumbnail-height :initarg :thumbnail-height :initform 200 :accessor thumbnail-height
		    :type (integer 50 400))
   (display-width :initarg :display-width :initform 500 :accessor display-width
		    :type (integer 200 1000))
   (display-height :initarg :display-height :initform 500 :accessor display-height
		    :type (integer 200 1000))
   (sku-prefix :initarg :sku-prefix :initform "SKU" :accessor sku-prefix :type (string 3))
   (sku-counter :initform 0 :accessor sku-counter :type integer)
   (order-prefix :initarg :order-prefix :initform "ORD" :accessor order-prefix :type (string 3))
   (order-counter :initform 0 :accessor order-counter :type integer))
  (:documentation "Singleton object designed to hold configuration
  items for an instance of souk"))

(defun get-next-sku ()
  (prog1
      (format nil "~A~7,'0d"
	      (sku-prefix *souk-configuration*)
	      (sku-counter *souk-configuration*))
    (incf (sku-counter *souk-configuration*))))


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
