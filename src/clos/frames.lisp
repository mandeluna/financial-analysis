(in-package :financial-analysis.clos)

;; The Class Hierarchy

;; Abstract base class
(define-frame asset ()
  ((name :initarg :name
	 :accessor asset-name)))

;; Security inherits from Asset
(define-frame security (asset)
  ((price :initarg :price
	  :accessor security-price
	  :units :usd
	  :range (0.0 nil))  ; price cannot be negative
			    (currency :initform "USD")))

;; Bond inherits from Security
(define-frame bond (security)
  ((coupon-rate :initarg :coupon-rate
		:accessor bond-coupon
		:initform 0.05    ; standard CLOS initfrom
		:range (0.0 0.30) ; frame constraint 0% to 30%
		:units :percent)

   (par-value   :initarg :par-value
		:accessor bond-par
		:default 1000.0   ; frame default
		:units :usd)

   (maturity    :initarg :maturity
		:accessor bond-maturity
		:units :years)

   (frequency   :initarg :frequency
		:accessor bond-frequency
		:default 1       ; frame default: annual payents
		:range (1 12))

   ;; Input: market yield
   (yield       :initarg :yield
		:accessor bond-yield
		:initform nil)

   ;; output: price (calculated on demand)
   (price       :initarg :price
		:accessor security-price
		:units :usd
		;; the logic
		:if-needed (lambda (b)
			     (if (bond-yield b)
				 (present-value b (bond-yield b))
				 (error "Cannot compute Price: Yield is missing"))))))

;;
;; The Generic protcol (polymorphism)
;;

(defgeneric cash-flows (object)
  (:documentation "Returns the stream of cash flows for an asset."))

(defgeneric present-value (object discount-rate)
  (:documentation "Calculates the PV of the object."))

;;
;; Methods
;;

;; Method for Bond
;; Notice: we don't pass 'years' or 'coupon' as arguments.
;; We ask the object (slot-value bond ...) for them.
(defmethod cash-flows ((b bond))
  (let* ((years (bond-maturity b))
	 (freq  (bond-frequency b))
	 (par   (bond-par b))
	 (rate  (bond-coupon b))

	 (periods (* years freq))
	 (coupon-amt (* par (/ rate freq)))

	 ;; create list of coupons
	 (flows (make-list periods :initial-element coupon-amt)))

    ;; Add principal to the last one
    (incf (car (last flows)) par)

    ;; Prepend 0 for time=0 (conceptually, you don't get paid the moment you buy)
    (cons 0 flows)))

;; Method for any asset that has cash flows
(defmethod present-value ((obj asset) discount-rate)
  (let ((flows (cash-flows obj))
	(period 0))
    (reduce #'+
	    (mapcar (lambda (cf)
		      (prog1
			(/ cf (expt (+ 1.0 discount-rate) period))
			(incf period)))
		    flows))))

