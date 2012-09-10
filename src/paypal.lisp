(in-package #:souk)

(defparameter *express-checkout-api-methods*
  '("SetExpressCheckout" ; Prepares the Express Checkout transaction
			 ; and defines the return URL (the path back
			 ; to the merchant's site
   "GetExpressCheckoutDetails" ; Returns order details and buyer and
			       ; shipping information (helpful for
			       ; review before executing the payment)
   "DoExpressCheckoutPayment" ; Completes an Express Checkout
			      ; transaction
))

(defun paypal-request (method profile &rest parameters)
  (assert (evenp (length parameters)))
  (let ((get-parameters (mapcar #'cons
				'("USER" "SIGNATURE" "VERSION" "PWD" "cancelUrl" "returnUrl")
				(with-slots (user signature version pwd cancelurl returnurl)
				    profile
				  (mapcar (lambda (slot)
					    (format nil "~A" slot))
					  (list user signature version pwd cancelurl returnurl))))))
    (flet ((add-parameter (key value)
	     (push (cons (string-upcase (format nil "~A" key))
			 (format nil "~A" value))
		   get-parameters)))
      (add-parameter 'method method)
      (dolist (elt (alexandria:plist-alist parameters))
	(add-parameter (car elt) (cdr elt))))
    (multiple-value-bind (response-text response-code)
	(drakma:http-request "https://api-3t.sandbox.paypal.com/nvp" 
			     :parameters get-parameters)
      (assert (= response-code 200))
      (get-parameters->plist response-text))))



(defpclass paypal-profile ()
  ((user :initarg :user :initform nil :accessor user :index t)
   (signature :initarg :signature :initform nil :accessor signature)
   (version :initarg :version :initform nil :accessor version)
   (pwd :initarg :pwd :initform nil :accessor pwd)
   (cancelurl :initarg :cancelurl :initform "http://www.yourdomain.com/cancel.html"
	      :accessor cancelurl)
   (returnurl :initarg :returnurl :initform "http://www.yourdomain.com/success.html"
	      :accessor returnurl))) 

(defparameter *test-paypal-profile* (test-paypal-profile))

(defun test-paypal-profile ()
  (make-instance 'paypal-profile
		 :user "soukie_1346541313_biz_api1.gmail.com"
		 :pwd "1346541342"
		 :signature "AJ8wGao2gZvTKX3KL69DRym1YabbAC90zuvWhc.Bv9XxfFUe2JMndL.o"
		 :version "78"))

(defun get-parameters->plist (params)
  (alexandria:alist-plist
   (mapcar (lambda (item)
	     (destructuring-bind (k v)
		 (cl-ppcre:split "=" item)
	       (cons (intern (string-upcase k) :keyword)
		     (canonicalize-url-encoded-string v))))
	   (cl-ppcre:split "&" params))))


(defun canonicalize-url-encoded-string (string)
  (cl-ppcre:regex-replace-all (cl-ppcre:create-scanner "%([0-9a-f]{2})") 
			      string
			      (lambda (match &rest registers)
				(declare (ignore match))
				(assert (= (length registers) 1))
				(format nil "~A" (code-char
						  (parse-integer (car registers) :radix 16))))
			      :simple-calls t))
