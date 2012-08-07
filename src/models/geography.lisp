(in-package #:souk)

(defpclass geography ()
  ((geography-name :initarg :name :accessor geo-name)
   (countries :initform '() :accessor countries)))

(defmethod print-object ((object geography) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (geo-name object) stream)))

(defun country-in (country geo)
  (member country (countries geo) :test #'string-equal))

(defun all-geographies (&rest args)
  (declare (ignore args))
  (find-persistent-objects (class-store 'geography) 'geography))


(defun print-geographies (item)
  (when (geographies item)
    (format nil "~{~A~^, ~}" (mapcar #'geo-name (geographies item)))))

(defun add-selection-to-geography (country geography)
  (with-slots (countries) geography
    (let ((selection (get-selection country)))
      (setf (countries geography)
	    (remove-duplicates (sort (append selection countries)
				     #'string<)
			       :test #'string-equal)))))

(defun remove-selection-from-geography (selection geography)
  (with-slots (countries) geography
    (let ((selection-countries (get-selection selection)))
      (setf (countries geography)
	    (set-difference countries selection-countries
			    :test #'string-equal)))))


(defparameter *country-groups*
  '(("Europe"
     "Albania" "Andorra" "Austria"
     
     "Bosnia and Herzegovina" "Belgium"
     "Bulgaria" "Belarus" 
     
     "Cyprus" "Czech Republic" "Germany" "Denmark"
     "Spain" "Finland" "France"
     "United Kingdom / Great Britain" "Georgia" "Guernsey"
     "Gibraltar" "Greenland"
    
     "Greece" "Croatia" "Hungary" "Ireland"
     "Isle of Man" "Iceland"
     "Italia" "Jersey" "Liechtenstein" "Lithuania"
     "Luxembourg" "Latvia"
     
     "Moldova" "Montenegro" "Macedonia"
     "Malta"
     
     "Netherlands" "Norway"
     
     "Poland" "Portugal" 
     "Romania" "Serbia" "Russian Federation" "Sweden"
     "Slovenia" "Slovakia"
     "San Marino" "Switzerland" "Ukraine"
     "Holy See (Vatican City State)")
    
    ("North America"
     "United States of America"
     "Canada"
     "Mexico")
    
    ("Latin America"
     "Argentina" "Bolivia" "Brazil"
     "Belize"
     "Chile" "Colombia" "Costa Rica"
     "Cuba" "Dominica"
     "Dominican Republic" "Ecuador" "Falkland Islands (Malvinas)" "Grenada"
     "French Guiana" "Guatemala"
     "Guyana" "Honduras" "Mexico" "Nicaragua" "Panama" "Paraguay"
     "Peru" "Paraguay" "Uruguay" "Venezuela")))
    
(defparameter *w3-countries*
  '(("Italy" . "IT") ("United States of America" . "US") ("China" . "CN")
    ("Andorra" . "AD") ("United Arab Emirates" . "AE") ("Afghanistan" . "AF")
    ("Antigua and Barbuda" . "AG") ("Anguilla" . "AI")
    ("Albania" . "AL")
    ("Armenia" . "AM") ("Netherlands Antilles" . "AN") ("Angola" . "AO")
    ("Antarctica" . "AQ") ("Argentina" . "AR")
    ("American Samoa" . "AS")
    ("Austria" . "AT") ("Australia" . "AU") ("Aruba" . "AW")
    ("Azerbaijan" . "AZ") ("Bosnia and Herzegovina" . "BA")
    ("Barbados" . "BB")
    ("Bangladesh" . "BD") ("Belgium" . "BE") ("Burkina Faso" . "BF")
    ("Bulgaria" . "BG") ("Bahrain" . "BH") ("Burundi" . "BI")
    
    ("Benin" . "BJ") ("Saint Barthelemy" . "BL") ("Bermuda" . "BM")
    ("Brunei Darussalam" . "BN") ("Bolivia" . "BO") ("Brazil" . "BR")
    ("Bahamas" . "BS")
    ("Bhutan" . "BT") ("Bouvet Island" . "BV")
    ("Botswana" . "BW") ("Belarus" . "BY") ("Belize" . "BZ")
    ("Canada" . "CA") ("Cocos (Keeling) Islands" . "CC")
    ("Central African Republic" . "CF")
    ("Congo" . "CG") ("Switzerland" . "CH") ("Cote d'Ivoire" . "CI")
    ("Cook Islands" . "CK") ("Chile" . "CL")
    ("Cameroon" . "CM")
    ("China" . "CN") ("Colombia" . "CO") ("Costa Rica" . "CR")
    ("Cuba" . "CU") ("Cape Verde" . "CV") ("Christmas Island" . "CX")
    
    ("Cyprus" . "CY") ("Czech Republic" . "CZ") ("Germany" . "DE")
    ("Djibouti" . "DJ") ("Denmark" . "DK") ("Dominica" . "DM")
    ("Dominican Republic" . "DO")
    ("Algeria" . "DZ") ("Ecuador" . "EC")
    ("Estonia" . "EE") ("Egypt" . "EG") ("Eritrea" . "ER")
    ("Spain" . "ES") ("Ethiopia" . "ET") ("Finland" . "FI")
    
    ("Fiji" . "FJ") ("Falkland Islands (Malvinas)" . "FK") ("Micronesia, Federated States of" . "FM")
    ("Faroe Islands" . "FO") ("France" . "FR")
    ("Gabon" . "GA")
    ("United Kingdom / Great Britain" . "GB") ("Grenada" . "GD") ("Georgia" . "GE")
    ("French Guiana" . "GF") ("Guernsey" . "GG")
    ("Ghana" . "GH")
    ("Gibraltar" . "GI") ("Greenland" . "GL") ("Gambia" . "GM")
    ("Guinea" . "GN") ("Guadeloupe" . "GP") ("Equatorial Guinea" . "GQ")
    
    ("Greece" . "GR") ("South Georgia and the South Sandwich Islands" . "GS") ("Guatemala" . "GT")
    ("Guam" . "GU") ("Guinea-Bissau" . "GW")
    ("Guyana" . "GY")
    ("Hong Kong" . "HK") ("Heard Island and McDonald Islands" . "HM") ("Honduras" . "HN")
    ("Croatia" . "HR")
    ("Haiti" . "HT") ("Hungary" . "HU")
    ("Indonesia" . "ID") ("Ireland" . "IE") ("Israel" . "IL")
    ("Isle of Man" . "IM") ("India" . "IN")
    ("British Indian Ocean Territory" . "IO")
    ("Iraq" . "IQ") ("Iran" . "IR") ("Iceland" . "IS")
    ("Italia" . "IT") ("Jersey" . "JE") ("Jamaica" . "JM")
    
    ("Jordan" . "JO") ("Japan" . "JP") ("Kenya" . "KE")
    ("Kyrgyzstan" . "KG") ("Cambodia" . "KH") ("Kiribati" . "KI")
    ("Comoros" . "KM")
    ("Saint Kitts and Nevis" . "KN") ("North Korea" . "KP")
    ("Republic of Korea" . "KR") ("Kuwait" . "KW") ("Cayman Islands" . "KY")
    
    ("Kazakhstan" . "KZ") ("Lao PDR" . "LA") ("Lebanon" . "LB")
    ("Saint Lucia" . "LC") ("Liechtenstein" . "LI") ("Sri Lanka" . "LK")
    
    ("Liberia" . "LR") ("Lesotho" . "LS") ("Lithuania" . "LT")
    ("Luxembourg" . "LU") ("Latvia" . "LV") ("Libya" . "LY")
    ("Morocco" . "MA")
    ("Monaco" . "MC") ("Moldova" . "MD")
    ("Montenegro" . "ME") ("Madagascar" . "MG") ("Marshall Islands" . "MH")
    ("Macedonia" . "MK")
    ("Mali" . "ML") ("Myanmar" . "MM")
    ("Mongolia" . "MN") ("Macao" . "MO") ("Northern Mariana Islands" . "MP")
    ("Martinique" . "MQ")
    ("Mauritania" . "MR") ("Montserrat" . "MS")
    ("Malta" . "MT") ("Mauritius" . "MU") ("Maldives" . "MV")
    ("Malawi" . "MW") ("Mexico" . "MX")
    ("Malaysia" . "MY")
    ("Mozambique" . "MZ") ("Namibia" . "NA") ("New Caledonia" . "NC")
    ("Niger" . "NE") ("Norfolk Island" . "NF")
    ("Nigeria" . "NG")
    ("Nicaragua" . "NI") ("Netherlands" . "NL") ("Norway" . "NO")
    ("Nepal" . "NP") ("Nauru" . "NR") ("Niue" . "NU")
    
    ("New Zealand" . "NZ") ("Oman" . "OM") ("Panama" . "PA")
    ("Peru" . "PE") ("French Polynesia" . "PF") ("Papua New Guinea" . "PG")
    
    ("Philippines" . "PH") ("Pakistan" . "PK") ("Poland" . "PL")
    ("Saint Pierre and Miquelon" . "PM") ("Pitcairn" . "PN")
    ("Puerto Rico" . "PR")
    ("Palestinian Territory" . "PS") ("Portugal" . "PT") ("Palau" . "PW")
    ("Paraguay" . "PY") ("Qatar" . "QA")
    ("Reunion" . "RE")
    ("Romania" . "RO") ("Serbia" . "RS") ("Russian Federation" . "RU")
    ("Rwanda" . "RW") ("Saudi Arabia" . "SA")
    ("Solomon Islands" . "SB")
    ("Seychelles" . "SC") ("Sudan" . "SD") ("Sweden" . "SE")
    ("Singapore" . "SG") ("Saint Helena" . "SH")
    ("Slovenia" . "SI")
    ("Svalbard and Jan Mayen" . "SJ") ("Slovakia" . "SK") ("Sierra Leone" . "SL")
    ("San Marino" . "SM")
    ("Senegal" . "SN") ("Somalia" . "SO")
    ("Suriname" . "SR") ("Sao Tome and Principe" . "ST") ("El Salvador" . "SV")
    
    ("Syrian Arab Republic" . "SY") ("Swaziland" . "SZ") ("Turks and Caicos Islands" . "TC")
    ("Chad" . "TD") ("French Southern Territories" . "TF")
    ("Thailand" . "TH")
    ("Tajikistan" . "TJ") ("Tokelau" . "TK") ("Turkmenistan" . "TM")
    ("Tunisia" . "TN") ("Tonga" . "TO") ("Turkey" . "TR")
    
    ("Trinidad and Tobago" . "TT") ("Tuvalu" . "TV") ("Taiwan" . "TW")
    ("Tanzania" . "TZ") ("Ukraine" . "UA") ("Uganda" . "UG")
    
    ("United States" . "US") ("Uruguay" . "UY") ("Uzbekistan" . "UZ")
    ("Holy See (Vatican City State)" . "VA")
    ("Saint Vincent and the Grenadines" . "VC") ("Venezuela" . "VE")
    ("Virgin Islands, British" . "VG") ("Virgin Islands, U.S." . "VI")
    ("Viet Nam" . "VN")
    ("Vanuatu" . "VU") ("Wallis and Futuna" . "WF") ("Samoa" . "WS")
    ("Yemen" . "YE") ("Mayotte" . "YT")
    ("South Africa" . "ZA")
    ("Zambia" . "ZM") ("Zimbabwe" . "ZW"))
  "A alist of W3C country names and their abbreviations/TLDs.")

(defun w3-selections ()
  (remove-duplicates (sort (append *w3-countries* *country-groups*)
			   #'string< :key #'car)
		     :test #'string-equal :key #'car))

(defun w3-selection-names ()
  (mapcar #'car (w3-selections)))

(defparameter *w3-selections*
  (w3-selections))

(defun get-country-group-list ()
  (remove-duplicates (sort (mapcar #'car *country-groups*) #'string<)
		     :test #'string-equal))

(defun get-country-list ()
  (remove-duplicates (sort (mapcar #'car *w3-countries*) #'string<)
		     :test #'string-equal))

(defun get-selection (selection)
  (let ((selections (cdr (assoc selection *w3-selections* :test #'string-equal))))
    (if (listp selections)
	selections
	(list selection))))



(defun country-code->name (co)
  (car (rassoc co *w3-countries* :test #'string-equal)))

(defun country-name->code (name)
  (cdr (assoc name *w3-countries* :test #'string-equal)))

(defun get-country-selections ()
  (mapcar #'car *w3-selections*))



(defun w3-country-p (str)
  "Used by the type specifier 'w3-country' to determine if a string is a
country."
  (or (country-name->code str)
      (country-code->name str)))

(deftype w3-country ()
  '(satisfies w3-country-p))

(defclass w3-country-presentation (input-presentation)
  ((input-id :initform (gensym)
	     :initarg :input-id
	     :accessor w3-country-presentation-input-id
	     :documentation "An input ID passed to suggest or
	     dropdown.")
   (choices-id :initform (gensym)
	       :initarg :choices-id
	       :accessor w3-country-presentation-choices-id
	       :documentation "A choices ID passed to suggest.")))

(defmethod render-view-field-value (value (presentation w3-country-presentation) 
				    (field form-view-field) (view form-view) widget obj
				    &rest args &key intermediate-values &allow-other-keys)
  (declare (ignore args))
  (multiple-value-bind (intermediate-value intermediate-value-p)
      (form-field-intermediate-value field intermediate-values)
    (let ((selections (get-country-selections))
	  (default-value (if intermediate-value-p
			     intermediate-value
			     value)))
      (render-suggest (view-field-slot-name field)
		      selections
		      :default-value default-value
;		      :welcome-name welcome-name
		      :max-length (input-presentation-max-length presentation)
		      :input-id (w3-country-presentation-input-id presentation)
		      :choices-id (w3-country-presentation-choices-id presentation)))))

(defclass w3-country-parser (parser)
  ((error-message :initform "valid nazione"))
  (:documentation "A parser designed to parse strings into
  a w3c country."))

(defmethod parse-view-field-value ((parser w3-country-parser) value obj
				   (view form-view) (field form-view-field) &rest args)
  (declare (ignore args))
  (if (empty-p value)
      (values t nil value)
      (if (country-name->code value)
	  (values t t value)
	  (values nil nil nil))))

(defmethod typespec->view-field-presentation ((scaffold form-scaffold)
					      (typespec (eql 'w3-country)) args)
  (values t (make-instance 'w3-country-presentation)))

(defmethod typespec->form-view-field-parser ((scaffold form-scaffold)
					     (typespec (eql 'w3-country)) args)
  (values t (make-instance 'w3-country-parser)))

