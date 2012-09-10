(in-package :souk)

(defwidget druid-entry ()
  ((data :accessor druid-entry-data
	 :initarg :data
	 :initform nil
	 :documentation "The data for this druid entry")
   (druid :accessor druid
	  :initarg :druid
	  :initform nil
	  :documentation "The druid in which this entry resides")
   (data-edit-function :accessor data-edit-function
		       :initarg :edit-function
		       :initform nil
		       :documentation "A function that will be called
		       with the druid data and should return a
		       widget to be rendered")
   (data-validate-function :accessor data-validate-function
			   :initarg :validate-function
			   :initform nil
			   :documentation "A function that will be
			   called with the druid data and should
			   return T if the data is valid, and NIL if
			   it is not.  May also return (as a second
			   value) a piece of text as an error message,
			   or a list of strings in the case of
			   multiple errors.")
   (data-summary-function :accessor data-summary-function
			  :initarg :summary-function
			  :initform nil
			  :documentation "A function that will be
			  called as part of rendering a summary page.
			  Should return a widget with summary
			  information.")))

(defun make-druid (on-complete &rest entries)
  (let ((druid (make-instance 'druid :data entries :on-complete on-complete)))
    (dolist (entry (druid-data druid))
      (setf (druid entry) druid))
    druid))

(defwidget druid ()
  ((data :type list
         :accessor druid-data
         :initarg :data
         :initform '()
         :documentation "The list of data widgets held by this druid. By default
         elements will be presented in an ordered fashion with one element
         per step.")
   (data-cursor :type integer
                :accessor druid-data-cursor
                :initarg :data-cursor
                :initform 0
                :documentation "Offset of the current position in the data list.")
   (on-complete :type (or function symbol)
                :accessor druid-on-complete
                :initarg :on-complete
                :documentation "A function designator holding the function
                that is to be called after the completion of the druid.

                The function will be called with one argument, the druid.

                Must be provided."))
  (:documentation "A widget that displays a series of data objects in separate steps."))


(defmethod druid-total-steps ((druid druid))
  "Returns the total steps of the druid, which is by default
  the number of data items plus one for the final confirmation
  page."
  (1+ (length (druid-data druid))))

(defmethod druid-processed-data ((druid druid))
  "Returns the data already processed so far by the druid.

  This is not a guarantee that the other items haven't been shown
  to the user but rather an indicator of which data contents can currently
  be considered valid."
  (safe-subseq (druid-data druid) 0 (druid-data-cursor druid)))

(defmethod druid-current-datum ((druid druid))
  "Returns the data object associated with the current druid state."
  (elt (druid-data druid) (druid-data-cursor druid)))
 
(defmethod druid-remaining-data ((druid druid))
  "Returns the data objects still remaining to be processed.

  The notes from DRUID-PROCESSED-DATA hold here, too."
  (nthcdr (druid-data-cursor druid) (druid-data druid)))

(defmethod druid-proceed ((druid druid))
  "Proceed to the next step. Callers mustn't attempt to go
  beyond the final step."
  (prog1
    (incf (druid-data-cursor druid))
    (assert (<= (druid-data-cursor druid) (druid-total-steps druid)))))

(defmethod druid-recede ((druid druid))
  "Recede to the previous step. Callers mustn't attempt to go
  beyond the first step (i.e. to step zero)."
  (prog1
    (decf (druid-data-cursor druid))
    (assert (>= (druid-data-cursor druid) 0))))

(defmethod druid-render-no-data ((druid druid))
  "Called to render a message indicating that this druid does
  not have any data to operate on."
  (with-html
    (:p "This Druid does not have any data to manipulate")))

(defmethod druid-render-status ((druid druid) (type (eql :simple)))
  "Render the current status of the druid as a simple text string
  displaying just the current step."
  (declare (ignore type))
  (with-html
    (:div :class "status"
      (esc (format nil "Step ~D" (1+ (druid-current-step druid)))))))

(defmethod druid-render-status ((druid druid) (type (eql :simple-with-total)))
  "Render the current status of the druid as a simple text string
  displaying the current step and the number of total steps."
  (declare (ignore type))
  (with-html
    (:div :class "status"
	  (esc (format nil "Step ~D/~D" (1+ (druid-current-step druid))
		       (druid-total-steps druid))))))

(defmethod druid-current-step ((druid druid))
  (druid-data-cursor druid))

(defmethod druid-total-steps ((druid druid))
  (length (druid-data druid)))

(defmethod druid-render-summary-page ((druid druid))
  "Render the final summary/confirmation page of the druid."
  (with-html
    (dolist (d (druid-data druid))
      (render-widget (funcall (data-summary-function d) (druid-entry-data d))))
    (:p :class "druid-summary-back"
	(render-link
	 (f_% (druid-recede druid))
	 "Back"))
    (:p :class "druid-summary-confirm"
	(render-link
	 (f_% (funcall (druid-on-complete druid) druid))
	 "Confirm"))))

(defmethod render-druid ((druid druid))
;  (with-html (:h1 "Druid begins"))
  (if (eql (druid-current-step druid) (druid-total-steps druid))
      (druid-render-summary-page druid)
      (with-html
	;; (:pre (esc (with-output-to-string (s)
	;; 	     (describe druid s))))
	;; (dolist (d (druid-data druid))
	;;   (htm (:pre (esc (with-output-to-string (s)
	;; 		    (describe d s))))))
	(druid-render-status druid :simple-with-total)
	(render-widget (druid-current-widget druid))))
  ;; (with-html (:h1 "Druid ends"))
  )



(defmethod druid-current-widget ((druid druid))
  (elt (druid-data druid)
       (druid-data-cursor druid)))

(defmethod render-widget-body ((druid druid) &rest args)
  "Render the druid."
  (declare (ignore args))
  (cond
    ((null (druid-data druid))
     (druid-render-no-data druid))
    (t
;     (druid-render-status druid (druid-status-type druid))
     (render-druid druid)
     )))

(defmethod render-widget-body ((druid-entry druid-entry) &rest args)
  (declare (ignore args))

  ;; (with-html
  ;;   (:h1 "Druid entry begins"))
  
  
  (render-widget (funcall (data-edit-function druid-entry) (druid-entry-data druid-entry)))

  ;; (with-html
  ;;   (:h1 "Druid ends"))

  ;; (with-html
  ;;   (:h1 "Druid nav begins"))
 
  (unless (zerop (druid-data-cursor (druid druid-entry)))
    (render-link (make-action (lambda (&rest args)
				(declare (ignore args))
				(decf (druid-data-cursor (druid druid-entry)))
				(mark-dirty (druid druid-entry))))
		 " << Back "))
  (unless (>= (druid-data-cursor (druid druid-entry)) (length (druid-data (druid druid-entry))))
    (render-link (make-action (lambda (&rest args)
				(declare (ignore args))
				(incf (druid-data-cursor (druid druid-entry)))
				(mark-dirty (druid druid-entry))))
		 " Next >> "))

    ;; (with-html
    ;; (:h1 "Druid nav ends"))
    )



;; (defmethod render-form-view-buttons ((view form-view) obj (widget druid-dataform) &rest args)
;;   "Render the buttons to recede and proceed with the druid."
;;   (declare (ignore obj args))
;;   (flet ((find-button (name)
;;            (ensure-list
;;              (find name (form-view-buttons view)
;;                    :key (lambda (item)
;;                           (car (ensure-list item)))))))
;;     (with-html
;;       (:div :class "submit"
;;             (unless (eql (druid-current-step (druid-dataform-druid widget)) 1)
;;               (let ((cancel (or (find-button :back) '(:back))))
;;                 (render-button *cancel-control-name*
;;                                :class "submit cancel"
;;                                :value (or (cdr cancel)
;;                                           (humanize-name (car cancel))))))
;;             (when (< (druid-current-step (druid-dataform-druid widget))
;;                      (druid-total-steps (druid-dataform-druid widget)))
;;               (let ((submit (or (find-button :next) '(:next))))
;;                 (render-button *submit-control-name*
;;                                :value (or (cdr submit)
;;                                           (humanize-name (car submit))))))))))
;; (defmethod initialize-instance :after ((inst druid) &rest initargs)
;;   "Initialize the druid's first page."
;;   (setf (druid-data inst)
;; 	(mapcar (lambda (datum)
;; 		  (funcall (data-edit-function datum) datum))
;; 		(druid-data inst))))



;; (defmethod (setf druid-data-cursor) :after (value (druid druid))
;;   "Updates the current widget when the data item changes."
;;   (declare (ignore value))
;;   (druid-update-current-widget druid))

;; (defmethod (setf druid-current-step) :after (value (druid druid))
;;   "Updates the current widget when the data item changes."
;;   (declare (ignore value))
;;   (druid-update-current-widget druid))

;; (defwidget druid-dataform (dataform)
;;   ((druid :type druid
;; 	  :accessor druid-dataform-druid
;; 	  :initarg :druid
;; 	  :documentation "The druid owning the dataform. You better set this
;;            at initialization time so the dataform can decide what buttons to show."))
;;   (:default-initargs :ui-state :form
;;     :allow-close-p nil)
;;   (:documentation "A dataform slightly customized for the druid's purposes."))

;; (defmethod initialize-instance :after ((form druid-dataform) &rest initargs)
;;   (let ((druid (druid-dataform-druid form)))
;;     (setf (dataform-on-cancel form) (f_% (if (> (druid-current-step druid) 1)
;;                                            (druid-recede druid)
;;                                            (mark-dirty druid))))
;;     (setf (dataform-on-success form) (f_% (if (< (druid-current-step druid)
;;                                                  (druid-total-steps druid))
;;                                             (druid-proceed druid)
;;                                             (mark-dirty druid))))))



;; (defmethod druid-create-widget ((druid druid) (step integer) data)
;;   "Create a fresh widget for the specified STEP and DATA.
;;   Default implementation creates a DRUID-DATAFORM."
;;   (make-instance 'druid-dataform :druid druid :data data
;;                  :form-view (druid-form-view druid data step)))

;; (defmethod druid-update-current-widget ((druid druid))
;;   "Instantiate the widget belonging to the current step
;;   and assign it to the druid."
;;   (when (druid-current-datum druid) ;; this is just a kludge as we don't need the
;;                                       ;; widget in the final step anyway
;;     (setf (druid-current-widget druid)
;;           (druid-create-widget druid (druid-current-step druid) (druid-current-datum druid)))))

;; (defmethod druid-render-step ((druid druid) (step integer) data)
;;   "Render a specific step. The default implementation either
;;   renders the final confirmation page (if the current step is the last step)
;;   or the current widget."
;;   (if (eql (druid-data-cursor druid) (length (druid-data druid)))
;;       (druid-render-summary-page druid)
;;       (render-widget (druid-current-widget druid))))
