(in-package #:souk)

(defun as-string (thing)
  (format nil "~A" thing))

(defun toggle-bos ()
  (setf *break-on-signals* (not *break-on-signals*)))

(defun bounce ()
  (progn (ql:quickload 'souk)
	 (weblocks:reset-sessions)))

(defun dump-description (d)
  (with-html
    (:pre (esc (with-output-to-string (s)
		 (describe d s))))))
