
(in-package :souk)

;; Define callback function to initialize new sessions
(defun init-user-session (root)
  (setf (widget-children root)
	(list (make-main-page))))

