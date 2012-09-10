(in-package #:souk)

(defun make-admin-page ()
  )
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

		       ;; (make-instance 'login :on-login (constantly nil))
		       (make-admin-navigation)
		       ;; (make-instance 'control-tabs
		       ;; 		      :tabs-dom-id "maintabs"
		       ;; 		      :tabs (list (cons "Admin" (make-admin-navigation))
		       ;; 				  (cons "View" (make-tabs-navigation))))
)))

;; (defun make-fw-example ()
;;   (form-widget-initialize-from-view (make-instance 'form-widget)
;; 				    'single-item-form-view))

;; (defun make-navigation-with-uri (name base-uri &rest args)
;;   "Instantiates the default navigation widget via 'make-instance' and
;; forwards it along with 'args' to 'init-navigation'. The navigation
;; widgets bears the title NAME."
;;   (let ((nav (apply #'make-instance (or (safe-getf args :navigation-class) 'navigation)
;;                                     :name name :base-uri base-uri
;;                                     (safe-getf args :extra-args))))
;;     (setf args (remove-keyword-parameters args :navigation-class :extra-args))
;;     (apply #'init-navigation nav args)
;;     nav))

(defun make-main-navigation ()
  (make-navigation
   'main-menu
   "Admin" (make-admin-navigation)
   "Tags" (make-tags-navigation)
   :extra-args (list :base-uri "main")))

(defun make-admin-navigation ()
  (make-navigation
   'main-admin-menu
   "Single items" (make-single-items-gridedit)
   "Bundles" (make-bundles-gridedit)
   ;; "Postal providers"
   ;; (make-provider-gridedit)
   "Geographies"
   (make-geography-gridedit)
   "Tags"
   (make-tags-gridedit)
   "Store configuration"
   (make-souk-configuration-editor)
   "View single items"
   (make-instance 'widget
		  :children (list (make-shopping-cart-widget :short)
				  (single-item-display-table 5)))
   "View shopping cart"
   (make-instance 'shopping-cart-widget :mode :full)))

(defmethod dataedit-create-drilldown-widget ((grid gridedit) (geo geography))
  (make-instance 'widget
		 :children (list (call-next-method)
				 (geography-selector geo))))

(defun make-tag-cloud-for-item (item)
  (make-instance 'tag-cloud
		 :tag-object item
		 :all-tags #'all-tags
		 :accessor #'tags
		 :test #'equalp
		 :remove-fn #'untag-item
		 :add-fn #'tag-item
		 :label-fn #'tag-name
		 :render-fn (lambda (tag)
			      (with-html (str tag)))
		 :widget-prefix-fn
		 (lambda (args)
		   (declare (ignore args))
		   (with-html (:h1 "Add and remove tags")))
		 :widget-suffix-fn
		 (lambda (w &rest args)
		   (declare (ignore args))
		   (with-html-form (:get (make-action (lambda (&key newtag action)
							(declare (ignore action))
							(if (find-tag-by-name newtag)
							    (tag-item tag item)
							    (let ((tag (make-instance 'tag :name newtag)))
							      (tag-item tag item)))
							(mark-dirty w))))
		     (:p "Add a new tag"
			 (:input :type "text" :name "newtag"))))))






(defmethod dataedit-create-drilldown-widget ((grid gridedit) (item single-item))
  (make-instance 'widget
		 :children (list (call-next-method)
				 (make-tag-cloud-for-item item)
				 (make-instance
				  'widget
				  :widget-prefix-fn
				  (lambda (args)
				    (declare (ignore args))
				    (with-html (:h1 "Add and remove images")))
				  :children (list (make-instance 'add-image-widget
								 :line-item item)
						  (make-instance 'display-images-widget
								 :line-item item))))))


(defmethod dataedit-create-drilldown-widget ((grid gridedit) (bundle bundle))
  (make-instance
   'control-tabs
   :tabs (list (cons "Edit"
		     (make-instance 'widget :children
				    (list (call-next-method)
					  (make-tag-cloud-for-item bundle)
					  (make-instance
					   'widget
					   :children (list (make-instance 'add-image-widget
									  :line-item bundle)
							   (make-instance 'display-images-widget
									  :line-item bundle))))))
	       
	       (cons "Add Items"
		     (make-bundle-items-gridedit bundle))
	       (cons "Edit items" (make-instance 'quantity-list-widget
						 :qlist bundle
						 :q-item-delete-p t
						 :q-item-add-p t
						 :q-item-render-fn
						 (lambda (item)
						   (list (title (qlist-entry-item item)))))))))





(defmethod dataedit-create-drilldown-widget ((grid gridedit) (provider provider))
  (make-instance
   'widget
   :children (list (call-next-method)
		   (make-instance 'quantity-list-widget
				  :qlist provider
				  :q-item-add-p t
				  :q-item-delete-p t))))



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

(defun make-souk-configuration-editor ()
  (make-instance 'dataform
		 :data *souk-configuration*
		 :allow-close-p nil
		 :data-class 'souk-configuration
		 ;; :view 'souk-configuration-table-view
		 ;; :item-form-view 'souk-configuration-form-view
		 ;; :allow-add-p nil
		 ;; :allow-delete-p nil
		 ))

(defun make-bundles-gridedit ()
  (make-instance 'stack-grid
		 :name 'bundle-grid
		 :data-class 'bundle
		 :view 'bundle-view
		 :item-data-view 'bundle-data-view
		 :item-form-view 'bundle-form-view))

(defun make-single-items-gridedit ()
  (make-instance 'stack-grid
		 :name 'single-items-grid
		 :data-class 'single-item
		 :view 'single-items-view
		 :item-data-view 'single-item-data-view
		 :item-form-view 'single-item-form-view
		 ;; :widget-prefix-fn (lambda (&rest args)
		 ;; 		     (declare (ignore args))
		 ;; 		     (with-html
		 ;; 		       (:h1 "Single item view")
		 ;; 		       (:p "This is the list of single items, stock-keeping units in this system.  To edit an item, click on it in the list below.  These items can be sold single, or grouped together in bundles.  If 'featured' is selected, the item can appear in lists of featured items on the front page of the webpage.  If 'published' is selected, the item will appear on the public webpage.  Deselect 'published' if you want to create or edit an item without it appearing on the website.")))
		 ))


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
  (make-instance 'stack-grid
		 :name 'geography-grid
		 :data-class 'geography
		 :view 'geography-view
		 :item-data-view 'geography-data-view
		 :item-form-view 'geography-form-view
		 :widget-prefix-fn (lambda (&rest args) (declare (ignore args))
					   (with-html (:h1 "Geography view")))))

(defun make-tags-gridedit ()
  (make-instance 'stack-grid
		 :name 'tags-grid
		 :data-class 'tag
		 :view 'tags-view
		 :item-data-view 'tags-data-view
		 :item-form-view 'tags-form-view))

;(defun make-geography-countries-list)



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


(defun single-item-display-table (&optional (items-per-page 10))
  (let ((grid (make-instance 'datagrid
			     :name 'single-item-display
			     :data-class 'single-item
			     :view 'single-item-display-view)))
    (setf (pagination-items-per-page (dataseq-pagination-widget grid))
	  items-per-page)
    grid))






