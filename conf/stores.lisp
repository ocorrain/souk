
(in-package :souk)

;;; Multiple stores may be defined. The last defined store will be the
;;; default.
(defstore *souk-store* :elephant
  :spec `(:bdb ,(namestring (merge-pathnames (make-pathname :directory '(:relative "data"))
					     (asdf-system-directory :souk)))))
