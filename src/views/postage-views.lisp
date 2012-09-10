(in-package #:souk)

(defview provider-view (:type table :inherit-from '(:scaffold provider))
  (items :hidep t)
  (geography :reader #'geo-printer))

(defview provider-data-view (:type data :inherit-from '(:scaffold provider)))

(defview provider-form-view (:type form :inherit-from '(:scaffold provider))
  (items :hidep t;; :present-as (checkboxes :choices #'get-postage-rates)
	 )
  (geography :reader #'geo-reader
	     :present-as (dropdown :choices #'all-geographies
				   :label-key #'geo-name)
	     :parse-as (object-id :class-name 'geography)))

(defview geography-view (:type table :inherit-from '(:scaffold geography))
  (countries :present-as (short-list :format-string "~{~A~^, ~}")))


(defview geography-data-view (:type data :inherit-from '(:scaffold geography)))

(defview geography-form-view
    (:type form :inherit-from '(:scaffold geography))
  (countries :present-as w3-country :writer #'add-selection-to-geography))