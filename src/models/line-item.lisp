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

(defgeneric get-price (item))

(defgeneric get-weight (item))

