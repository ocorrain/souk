(in-package #:souk)

(defwidget stack-grid (gridedit)
  ())

(defmethod render-widget-body ((obj stack-grid) &rest args)
  (declare (ignore args))

  (flet ((return-widget ()
	   (make-widget (lambda ()
			  (render-link (lambda (&rest args )
					 (declare (ignore args))
					 (dataedit-reset-state obj))
				       "return to grid" :class "button")))))
    
    (dataedit-update-operations obj)

    (if (dataedit-item-widget obj)
	(progn 
	  ;; (when (widget-parent (dataedit-item-widget obj))
	  ;;   (setf (widget-parent (dataedit-item-widget obj)) nil))
	  (render-widget 
	   (make-instance 'widget
			  :children (list 
				     (return-widget)
				     (dataedit-item-widget obj)))))
	(call-next-method))))




;; (defun make-stack (&rest args)
;;   (make-instance 'stack-widget :stack args))

;; (defun pop-widget-stack (stack-widget)
;;   (pop (stack stack-widget)))

;; (defmethod dataedit-create-drilldown-widget ((stack-widget stack-widget) item)
;;   (with-slots (stack) stack-widget
;;     (when stack
;;       (setf (stack stack-widget)
;; 	    (cons (dataedit-create-drilldown-widget (car stack) item)
;; 		  stack))
;;       (mark-dirty stack-widget))))

