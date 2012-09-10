(in-package #:souk)

(defview customer-view (:type form :inherit-from '(:scaffold customer))
  (cookie :hidep t)
  (address :present-as (textarea :rows 5))
  (country :present-as (dropdown :choices (mapcar #'car *w3-countries*)))
  (password :hidep t)
  (orders :hidep t))
