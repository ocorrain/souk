(in-package #:souk)

(defwidget tag-cloud ()
  ((tag-cloud-object :accessor tag-cloud-object :initarg :tag-object)
   (all-tags :accessor tag-cloud-all-tags :initarg :all-tags
	     :documentation "Function of no arguments returning all of the valid tags")
   (accessor :accessor tag-cloud-accessor :initarg :accessor
	     :documentation "Most take the tag-cloud-object as an
	     argument and return a list of the object's current tags")
   (member-test :accessor tag-cloud-member-test :initarg :test :initform #'equal)
   (member-key :accessor tag-cloud-member-key :initarg :key :initform #'identity)
   (remove-function :accessor tag-cloud-remove-function :initarg :remove-fn)
   (add-function :accessor tag-cloud-add-function :initarg :add-fn)
   (label-function :accessor tag-cloud-label-function :initarg :label-fn :initform #'princ-to-string)
   (title-function :accessor tag-cloud-title-function :initarg :title-fn :initform #'princ-to-string)
   (render-function :accessor tag-cloud-render-function :initarg :render-fn :initform #'princ-to-string)))

(defmethod render-widget-body ((obj tag-cloud) &rest args)
  (declare (ignore args))
  (with-slots (tag-cloud-object accessor member-test member-key remove-function
				add-function label-function title-function
				render-function all-tags)
      obj
    (when-let (tags (funcall accessor tag-cloud-object))
      (with-html
	(:ul :class "tag-cloud"
	     (dolist (tag (funcall all-tags))
	       (htm (:li :class "tag-cloud-item"
			 (if (member tag tags :test member-test :key member-key)
			     (render-link (make-action (lambda (&rest args)
							 (funcall remove-function
								  tag tag-cloud-object)
							 (mark-dirty obj)))
					  (funcall label-function tag)
					  :class "tag-cloud-selected"
					  :title (funcall title-function tag)
					  :render-fn render-function)


			     (render-link (make-action (lambda (&rest args)
							 (funcall add-function
								  tag tag-cloud-object)
							 (mark-dirty obj)))
					  (funcall label-function tag)
					  :class "tag-cloud-unselected"
					  :title (funcall title-function tag)
					  :render-fn render-function))))))))))

