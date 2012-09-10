(in-package #:souk)


(defview tags-view (:type table :inherit-from '(:scaffold tag)))

(defview tags-data-view (:type data :inherit-from '(:scaffold tag)))

(defview tags-form-view (:type form :inherit-from '(:scaffold tag)))
