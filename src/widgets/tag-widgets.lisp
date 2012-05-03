(in-package :souk)

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