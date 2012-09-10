(in-package #:souk)

(defpclass customer ()
  ((cookie :initarg :cookie :accessor cookie :type string :index t
	   :initform (hunchentoot::create-random-string 20 20))
   (name :initarg :name :accessor name :type string)
   (address :initarg :address1 :accessor address1 :type string)
   (country :initarg :country :accessor country :type string)
   (postcode :initarg :postcode :accessor postcode :type string :initform "")
   (email :initarg :email :accessor email :type string :index t)
   (phone :initarg :phone :accessor phone :type string)
   (password :accessor password :type string)
   (orders :accessor orders :type list :initform nil)))

(defun get-or-initialize-customer ()
  (let ((customer-var (webapp-session-value "customer")))
    (if (and customer-var (typep customer-var 'customer))
	customer-var
	(let ((customer-cookie (cookie-in "soukustomer")))
	  (if customer-cookie
	      (let ((customer (find-customer-by-cookie customer-cookie)))
		(if customer
		    customer
		    (initialize-customer)))
	      (initialize-customer))))))

(defun initialize-customer ()
  (let ((customer (make-instance 'customer)))
    (set-cookie "soukart" :value (cookie customer))
    (setf (webapp-session-value "customer") customer)
    customer))

(defun find-customer-by-cookie (cookie)
  (ele:get-instance-by-value 'customer 'cookie cookie))
