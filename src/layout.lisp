(in-package #:souk)

(defun make-main-page ()
  (make-instance 'widget :children
		 (list ;; (make-instance 'breadcrumbs
		       ;; 		      :widget-prefix-fn (lambda (&rest args)
		       ;; 					  (declare (ignore args))
		       ;; 					  (with-html
		       ;; 					    (:b "Breadcrumbs")))
		       ;; 		      :widget-suffix-fn (lambda (&rest args)
		       ;; 					  (declare (ignore args))
		       ;; 					  (with-html (:hr))))
		       ;(make-fw-example)
;		       (make-single-items-gridedit)
		       (make-navigation
		       	'main-admin-menu
		       	"Single items" (make-single-items-gridedit)
			"Bundles" (make-bundles-gridedit)
		       	"Postal providers"
		       	(make-provider-gridedit)
		       	"Geographies"
		       	(make-geography-gridedit)
			"Store configuration"
			(make-souk-configuration-gridedit)
			"View single items"
			(make-instance 'widget
				       :children (list (make-shopping-cart-widget :full)
						       (single-item-display-table))))
		       ;; (make-instance 'login :on-login (constantly nil))
		       )))

;; (defun make-fw-example ()
;;   (form-widget-initialize-from-view (make-instance 'form-widget)
;; 				    'single-item-form-view))

(defmethod dataedit-create-drilldown-widget ((grid gridedit) (item single-item))
  (make-instance
   'control-tabs
   :tabs (list (cons "Edit" (call-next-method))
	       (cons "Tags" (make-instance 'tags :tag-object item))
	       (cons "Images"
		     (make-instance
		      'widget
		      :children (list (make-instance 'add-image-widget
						     :line-item item)
				      (make-instance 'display-images-widget
						     :line-item item)))))))

(defmethod dataedit-create-drilldown-widget ((grid gridedit) (item bundle))
  (make-instance
   'control-tabs
   :tabs (list (cons "Edit" (call-next-method))
	       (cons "Tags" (make-instance 'tags :tag-object item))
	       (cons "Images"
		     (make-instance
		      'widget
		      :children (list (make-instance 'add-image-widget
						     :line-item item)
				      (make-instance 'display-images-widget
						     :line-item item))))
	       (cons "Add Items"
		     (make-bundle-items-gridedit item))
	       (cons "Edit items" (lambda (&rest args)
				    (declare (ignore args))
				    (with-html (:ul (dolist (i (items item))
						      (htm (:li (esc (title (car i)))))))))))))

(defun make-bundle-items-gridedit (bundle)
  (make-instance 'gridedit
		 :name 'bundle-items-grid
		 :view 'bundle-items-view
		 :data-class 'single-item
		 :allow-delete-p nil
		 :allow-add-p nil
		 :allow-drilldown-p nil
		 :allow-select-p t
		 :item-ops (list (cons "Add to bundle"
				       (lambda (grid selection)
					 (let ((selected-objects
						(mapcar (lambda (id)
							  (find-persistent-object-by-id
							   (dataseq-class-store grid)
							   (dataseq-data-class grid)
							   id))
							(cdr selection))))
					   (dolist (o selected-objects)
					     (add-item o bundle 1))))))))

(defun make-souk-configuration-gridedit ()
  (make-instance 'gridedit
		 :name 'souk-configuration-grid
		 :data-class 'souk-configuration
		 :view 'souk-configuration-table-view
		 :item-form-view 'souk-configuration-form-view
		 :allow-add-p nil
		 :allow-delete-p nil))

(defun make-bundles-gridedit ()
  (make-instance 'gridedit
		 :name 'bundle-grid
		 :data-class 'bundle
		 :view 'bundle-view
		 :item-data-view 'bundle-data-view
		 :item-form-view 'bundle-form-view))

(defun make-single-items-gridedit ()
  (make-instance 'gridedit
		 :name 'single-items-grid
		 :data-class 'single-item
		 :view 'single-items-view
		 :item-data-view 'single-item-data-view
		 :item-form-view 'single-item-form-view
		 :widget-prefix-fn (lambda (&rest args)
				     (declare (ignore args))
				     (with-html
				       (:h1 "Single item view")
				       (:p "This is the list of single items, stock-keeping units in this system.  To edit an item, click on it in the list below.  These items can be sold single, or grouped together in bundles.  If 'featured' is selected, the item can appear in lists of featured items on the front page of the webpage.  If 'published' is selected, the item will appear on the public webpage.  Deselect 'published' if you want to create or edit an item without it appearing on the website.")))))

(defun make-provider-gridedit ()
  (make-instance 'gridedit
		 :name 'provider-grid
		 :data-class 'provider
		 :view 'provider-view
		 :item-data-view 'provider-data-view
		 :item-form-view 'provider-form-view
		 :widget-prefix-fn (lambda (&rest args) (declare (ignore args))
					   (with-html (:h1 "Provider view")
						      (:p "This is the list of postal providers in the system.  Each geographical region should have at least one postal provider.  Providers represent different ways of getting goods from the supplier to the destination: for example, via courier, plain old post, or carrier pigeon.")))))

(defun make-geography-gridedit ()
  (make-instance 'gridedit
		 :name 'geography-grid
		 :data-class 'geography
		 :view 'geography-view
		 :item-data-view 'geography-data-view
		 :item-form-view 'geography-form-view
		 :widget-prefix-fn (lambda (&rest args) (declare (ignore args))
					   (with-html (:h1 "Geography view")))))

(defun make-admin-page ()
  (make-instance 'widget
		 :children (list (make-single-items-gridedit)
				(lambda (&rest args)
				  (declare (ignore args))
				  (with-html (:hr)))
				(make-geography-gridedit)
				(lambda (&rest args)
				  (declare (ignore args))
				  (with-html (:hr)))
				(make-provider-gridedit))))


(defun single-item-display-table ()
  (make-instance 'datagrid
		 :name 'single-item-display
		 :data-class 'single-item
		 :view 'single-item-display-view))

(defun make-mini-shopping-cart (item)
  (make-instance 'mini-shopping-cart-widget
		 :item item :cart (get-or-initialize-cart)))

(defun make-table-item-widget (item)
  (make-instance 'widget
		 :children (list (make-instance 'item-widget :item item)
				 (make-mini-shopping-cart item))))

(defun make-shopping-cart-widget (mode)
  (make-instance 'shopping-cart-widget
		 :shopping-cart (get-or-initialize-cart)
		 :display-mode mode))

