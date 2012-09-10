(in-package #:souk)

(defwidget quantity-list-widget ()
  ((qlist :initarg :qlist :initform nil :accessor qlist)
   (q-item-render-fn :initarg :q-item-render-fn
		     :initform #'identity
		     :accessor q-item-render-fn)
   (q-item-column-fn :initarg :q-item-column-fn :initform ""
		     :accessor q-item-column-fn)
   (q-item-delete-p :initarg :q-item-delete-p :accessor q-item-delete-p
		    :initform nil)
   (q-item-add-p :initarg :q-item-add-p :initform nil
		 :accessor q-item-add-p)))

(defmethod render-widget-body ((obj quantity-list-widget) &rest args)
  (declare (ignore args))
  (with-slots (qlist q-item-render-fn q-item-column-fn) obj
    (with-html ;; (mapcar q-item-column-fn (items qlist))
      (:table (dolist (i (items qlist))
		(htm 
		 (:tr
		  
		  (when (q-item-add-p obj)
		    (htm (:td (render-link (make-action
					    (lambda (&rest args)
					      (declare (ignore args))
					      (subtract-items (qlist-entry-item i) qlist 1)
					      (mark-dirty obj)))
					   "-"))
			 (:td (str (qlist-entry-quantity i)))
			 (:td (render-link (make-action
					    (lambda (&rest args)
					      (declare (ignore args))
					      (add-item (qlist-entry-item i) qlist 1)
					      (mark-dirty obj)))
					   "+"))))
		  (dolist (r (funcall q-item-render-fn i))
		    (htm (:td (str r))))
		  (when (q-item-delete-p obj)
		    (htm (:td (render-link (make-action
					    (lambda (&rest args)
					      (declare (ignore args))
					      (remove-item
					       (qlist-entry-item
						i) qlist)
					      (mark-dirty obj))) 
					   "delete"))))))))
      
      ;; (when (q-item-add-p obj)
      ;; 	(with-html-form (:get (make-action (lambda (&rest args &key item quantity &allow-other-keys)
      ;; 						     (declare (ignore args))
      ;; 						     (when (integerp (parse-integer quantity))
      ;; 						       (add-item item qlist
      ;; 								 (parse-integer quantity)))
      ;; 						     (mark-dirty obj))))
      ;; 	  (:input :type "text" :name "item")
      ;; 	  (:input :type "text" :name "quantity")
      ;; 	  (:input :type "submit")))
      )))


(defmethod render-qlist ((qlist quantity-list) &key (qrender #'identity) (irender #'identity))
  (with-table ("Quantity" "Item")
    (dolist (i (items qlist))
      (htm (:tr (:td (str (funcall qrender (qlist-entry-quantity i))))
		(:td (str (funcall irender (qlist-entry-item i)) )))));;  (:ul (dolist (i (items qlist))
    ;; (htm (:li (esc (title (car i)))))))
    )) 
 
(defmethod qlist->table ((qlist quantity-list) header-function footer-function item-function)
  "Renders a quantity list as a HTML table.
      HEADER-FUNCTION takes one argument, the qlist object.  It should
      return a list, or nil.  If this function returns, a THEAD
      element will be created, one column for each element in the list
      returned by HEADER-FUNCTION)

      ITEM-FUNCTION will be called on each item in the qlist, and
      should return a list of objects to be passed to STR.  One cell
      will be created for each item in the list.

      FOOTER-FUNCTION will be called with one argument, as with
      HEADER-FUNCTION.  If this returns a list of strings, it will be
      used to populate a TFOOT element."
  (object->table qlist #'items
		 :header-function  header-function :footer-function footer-function
		 :item-function item-function))

(defun object->table (obj get-items
		      &key header-function footer-function (item-function #'identity))
  "Renders a object as a HTML table.
      HEADER-FUNCTION takes the object to be rendered, OBJ.  It should
      return a list, or nil.  If this function returns, a THEAD
      element will be created, one column for each element in the list
      returned by HEADER-FUNCTION)

      GET-ITEMS should produce a list of items, one for each data row
      of the table.

      ITEM-FUNCTION will be called on each item returned by GET-ITEMS and
      should return a list of objects to be passed to STR.  One cell
      will be created for each item in the list.

      FOOTER-FUNCTION will be called with one argument, as with
      HEADER-FUNCTION.  If this returns a list of strings, it will be
      used to populate a TFOOT element."
  (when-let (rows (mapcar item-function (funcall get-items obj)))
    (with-html
      (:table (when-let (header (and header-function (funcall header-function obj)))
		(htm (:thead
		      (:tr (dolist (entry header)
			     (htm (:th (str entry))))))))
	      (when-let (footer (and footer-function (funcall footer-function obj)))
		(htm (:tfoot
		      (:tr (dolist (entry footer)
			     (htm (:th (str entry))))))))
	      (:tbody (dolist (row rows)
			(htm (:tr (dolist (col row)
				    (htm (:td (str col))))))))))))

