(in-package #:souk)

;; (defwidget display-grid-widget ()
;;   ((items :accessor items :initform nil :initarg :items)
;;    (row-size :accessor row-size :initform 5 :initarg :row-size))
;;   (:documentation "This is designed ))
(defun print-price (price-in-cents)
  (multiple-value-bind (euro cent)
      (floor price-in-cents 100)
    (format nil "€~:D.~2,'0D" euro cent)))

(defwidget item-widget ()
  ((item :accessor item :initform nil :initarg :item)))

(defmethod render-widget-body ((obj item-widget) &rest args)
  (declare (ignore args))
  (with-slots (item) obj
    (with-html (:h1 (esc (title item)))
	       (:p (:i (str (print-price (get-price item)))))
	       (:p (esc (short-description item))))))

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

(defwidget add-image-widget ()
  ((line-item :accessor line-item :initform nil :initarg :line-item))
  (:documentation "Class will present a simple HTML form to upload a
  new image, resize it, and add it to the image catalogue for the data-object"))

(defmethod render-widget-body ((obj add-image-widget) &rest args)
  (declare (ignore args))
  (with-slots (line-item) obj
    (with-html-form (:post (make-action (lambda (&key upload action)
					  (declare (ignore action))
					  (destructuring-bind (path filename content-type)
					      upload
					    (add-image path filename line-item))))
			   :enctype "multipart/form-data" :use-ajax-p nil)
      (:input :type "file" :name "upload")
      (:input :type "submit" :value "Upload"))))

(defwidget display-images-widget ()
  ((line-item :accessor line-item :initform nil :initarg :line-item))
  (:documentation "Display the images associated with the line item in
  a simple gallery.  This should be extended to allow extra
  functionality (e.g. deleting images)"))

(defmethod render-widget-body ((obj display-images-widget) &rest args)
  (declare (ignore args))
  (with-slots (line-item) obj
    (display-images line-item)))


(defwidget tags ()
  ((tag-object :accessor tag-object
	       :initform nil
	       :initarg :tag-object
	       :documentation "The object to be tagged/untagged"))
  (:documentation "Class that represents an object to be tagged, or
  untagged.  Will consist of three different things: first, a list of
  links to tags that are already associated with the object.  Clicking
  one of these will remove the tag from the object.  Second, a form
  field with a submit button, where the user can enter an additional
  tag that will be created de novo and associated with the
  object. Third: a list of links to tags that are not associated with
  the object.  Clicking one of these will associate it with the
  object.  Doing any of these actions will cause the widget to be
  redrawn."))

(defmethod render-widget-body ((obj tags) &rest args)
  (declare (ignore args))
  (with-slots (tag-object) obj
    (when (tags tag-object)
      (with-html
	(:p (:b "Click a tag to remove it: ")
	    (:ul (dolist (tag (tags tag-object))
		   (htm (:li (render-link (lambda (&rest args)
				  (declare (ignore args))
				  (setf (tags tag-object) (remove tag (tags tag-object)))
				  (mark-dirty obj))
				(tag-name tag)))))))))
    (with-html-form (:get (make-action (lambda (&key newtag action)
					 (declare (ignore action))
    					 (if (find-tag-by-name newtag)
    					     (push (find-tag-by-name newtag)
    						   (tags tag-object))
    					     (let ((tag (persist-object (class-store 'tag)
    									(make-instance 'tag :name newtag))))
    					       (push tag (tags tag-object))))
    					 (mark-dirty obj))))
      (:input :type "text" :name "newtag"))
    (with-html (:hr)
	       (:p (:b "Click a tag to add it: ")
		   (:ul (dolist (tag (remove-if (lambda (tag)
						  (member tag (tags tag-object) :test #'equalp))
						(find-persistent-objects (class-store 'tag) 'tag)))
			  (htm (:li (render-link (lambda (&rest args)
					 (declare (ignore args))
					 (push tag (tags tag-object))
					 (mark-dirty obj))
				       (tag-name tag))))))))))


(defwidget control-tabs ()
  ((tabs-dom-id :accessor tabs-dom-id :initarg :tabs-dom-id :initform "tabs")
   (tabs :accessor tabs :initarg :tabs :initform nil
	 :documentation "An alist of the tab titles and widgets to be rendered in the tab body")))

(defmethod render-widget-body ((obj control-tabs) &rest args)
  (declare (ignore args))
  (with-slots (tabs tabs-dom-id) obj
    (with-html
      ((:ul :id tabs-dom-id :class "subsection_tabs")
	(dolist (tab tabs)
	  (htm (:li :class "tab"
		    (:a :href (format nil "#~A" (attributize-name (car tab)))
			(esc (humanize-name (car tab))))))))
       (dolist (tab tabs)
	 (htm ((:div :id (format nil "~A" (attributize-name (car tab))))
	       (render-widget (cdr tab))))))
    (send-script (format nil "new Control.Tabs('~A');" tabs-dom-id))))

;; (defun tabs (alist &key component-namespace label)
;;   "ALIST is a list of keys and closures.  TABS outputs a closure that
;; takes a stream as an argument"
;;   (lambda (stream)
;;     (labels ((prefix (text)
;; 	       (add-prefix (get-webform text) component-namespace))
	     
;; 	     (tab-anchor (text)
;; 	       (concatenate 'string "#" (prefix text))))
      
;;       (with-html-output (s stream :indent t)
;; 	((:div :class "column span-24 last" :id "tab-set")
;; 	 ((:ul :class "tabs")
;; 	  (when label
;; 	    (htm ((:li :class "label") (str label))))
;; 	  (:li ((:a :href (tab-anchor (caar alist)) :class "selected") (str (caar alist))))
;; 	  (dolist (alist-entry (cdr alist))
;; 	    (htm (:li ((:a :href (tab-anchor (car alist-entry)))
;; 		       (str (car alist-entry)))))))

;; 	 (dolist (alist-entry alist)
;; 	   (htm ((:div :id (prefix (car alist-entry)))
;; 		 (str (funcall (cdr alist-entry) s))))))))))


;; (defun add-prefix (symbol-or-string prefix)
;;   (typecase symbol-or-string
;;     (symbol (add-prefix (string-downcase (symbol-name symbol-or-string)) prefix))
;;     (t (if prefix
;; 	   (format nil "~A:~A" prefix symbol-or-string)
;; 	   symbol-or-string))))

;; (defun definition-list (alist stream)
;;   (with-html-output (s stream)
;;     (:dl (dolist (item alist)
;; 	   (htm (:dt (str (car item)))
;; 		(:dd (str (cdr item))))))))

;; (defun fix-alist (alist)
;;   "Fixes the alist so that string keys are turned into symbols.  No
;;   casing is done, so the symbols will, in most lisp implementations,
;;   end up uppercased"
;;   (mapcar (lambda (p)
;; 	    (cons (read-from-string (car p))
;; 		  (cdr p)))
;; 	  alist))

