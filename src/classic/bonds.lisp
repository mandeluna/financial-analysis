(in-package :financial-analysis.classic)

;;; --- Numerical Utilities ---

(defun bisection-search (func target min max &key (tolerance 0.0001) (max-iter 100))
  "Finds x such that func(x) approx target using bisection method."
  (let ((iter 0))
    (loop
      (incf iter)
      (let* ((mid (/ (+ min max) 2.0))
             (val (funcall func mid)))
        (cond
          ;; Case 1: Found it (within tolerance)
          ((< (abs (- val target)) tolerance)
           (return mid))
          ;; Case 2: Give up (prevent infinite loop)
          ((> iter max-iter)
           (warn "Max iterations reached. Returning best guess.")
           (return mid))
          ;; Case 3: Narrow the search
          ((< val target) (setf max mid)) ;; Assumes inverse relationship (like Price/Yield)
          (t (setf min mid)))))))

;;; --- Financial Logic ---

(defun bond-cash-flows (coupon-rate par-value years &key (frequency 1))
  "Generates a list of cash flows for a standard fixed-rate bond."
  (let* ((periods (* years frequency))
         (coupon-payment (* par-value (/ coupon-rate frequency)))
         (flows (make-list periods :initial-element coupon-payment)))
    ;; Add the principal repayment to the final cash flow
    (setf (car (last flows)) (+ (car (last flows)) par-value))
    flows))

(defun bond-price (coupon-rate par-value years yield &key (frequency 1))
  "Calculates the price of a bond given a yield."
  (let ((flows (bond-cash-flows coupon-rate par-value years :frequency frequency)))
    ;; Reuse your NPV function. Note: Yield must be adjusted for frequency!
    ;; We add a 0 to the cash flows because first coupon is paid at tne end of period 1
    (net-present-value (cons 0 flows) (/ yield frequency))))

(defun internal-rate-of-return (cash-flows &key (guess 0.1))
  "Calculates IRR where NPV = 0."
  ;; We search for a rate that makes NPV of flows equal to 0.
  ;; Since NPV is inversely related to rate, bisection works well.
  ;; Note: This is a simplified solver assuming standard investment flows.
  (bisection-search (lambda (r) (net-present-value cash-flows r))
                    0.0     ; Target NPV
                    -0.99   ; Min Rate (-99%)
                    2.0))   ; Max Rate (200%)

(defun yield-to-maturity (current-price coupon-rate par-value years &key (frequency 1))
  "Calculates the YTM of a bond."
  (let ((target-price current-price))
    (bisection-search 
     (lambda (y) (bond-price coupon-rate par-value years y :frequency frequency))
     target-price
     0.0    ; Min Yield
     1.0))) ; Max Yield (100%)

(defun macaulay-duration (coupon-rate par-value years yield &key (frequency 1))
  "Calculates the weighted average time to receive cash flows."
  (let* ((flows (bond-cash-flows coupon-rate par-value years :frequency frequency))
         (price (bond-price coupon-rate par-value years yield :frequency frequency))
         (periodic-yield (/ yield frequency))
         (weighted-times 0)
         (period 1))
    
    (dolist (cf flows)
      (let ((pv-cf (present-value cf periodic-yield period)))
        (incf weighted-times (* period pv-cf))
        (incf period)))
    
    ;; Duration in periods / frequency = Duration in Years
    (/ (/ weighted-times price) frequency)))

;;
;; Constructor and accessors for Bond Frame
;;
(defun create-bond (name &rest proplist)
  (setf (symbol-plist name) nil)
  (create-node-aux name proplist)
  name)

(defun bond-coupon-rate (bond)
  (my-get bond :coupon-rate))

(defun bond-face-value (bond)
  (my-get bond :face-value))

(defun bond-call-value (bond)
  (my-get bond :call-value))

(defun bond-call-condition (bond)
  (my-get bond :call-condition))

(defun bond-life (bond)
  (my-get bond :life))

(defun bond-sd (bond)
  (my-get bond :sd))

(defun bond-discount-rate (bond)
  (my-get bond :discount-rate))

(defun node-payment (node)
  (my-get node :payment))

(defun node-call-condition (node)
  (my-get node :call-condition))

(defun node-discount-rate (node)
  (my-get node :discount-rate))

(defun node-coupon-rate (node)
  (my-get node :coupon-rate))

(defun node-face-value (node)
  (my-get node :face-value))

;;
;; Special functions needed for bond valuation
;; (*node-translation-table* see options.lisp)
(defun simple-cutoff (node)
  (let
      ((discount-rate (node-discount-rate node))
       (coupon-rate (node-coupon-rate node)))
    (< discount-rate coupon-rate)))

(defun par (node)               ; used as call price function
  (node-face-value node))

(defun nocall (node)
  nil)

;;
;; Decision tree for bonds and callable bonds
;;
(defun build-bond-tree (bond n)
  (let*
      ((call-node-nucleus-symbol (gensym))
       (chance-node-nucleus-symbol (gensym))
       (time (float (bond-life bond)))
       (period-length (/ time n))
       (coupon-rate (* (bond-coupon-rate bond) period-length))
       (face-value (bond-face-value bond))
       (coupon-payment (* coupon-rate face-value))
       (call-condition (bond-call-condition bond))
       (call-value (bond-call-value bond))
       (rate (* (bond-discount-rate bond) period-length))
       (sd (* (bond-sd bond) (sqrt period-length))))
    (create-node-nucleus chance-node-nucleus-symbol
			 :type 'chance
			 :exercise-function call-value
			 :face-value face-value
			 :upprob 0.5
			 :upamt sd
			 :downamt (- sd)
			 :successor-nucleus-symbol call-node-nucleus-symbol
			 :successors
			 '((symbol :discount-rate
			    (propor-change discount-rate upamt)
				   :rate (propor-change discount-rate upamt)
				   :prob upprob :left left)
			   (symbol :discount-rate
			    (propor-change discount-rate downamt)
				   :rate (propor-change discount-rate downamt)
				   :prob (- 1 upprob) :left left)))
    (create-node-nucleus call-node-nucleus-symbol
			 :type 'chance
			 :exercise-function call-value
			 :call-condition call-condition
			 :payment coupon-payment
			 :face-value face-value
			 :coupon-rate coupon-rate
			 :successor-nucleus-symbol chance-node-nucleus-symbol
			 :successors
			 '((symbol :type 'terminal             ; overrides chance type
				   :discount-rate discount-rate
				   :payoff payment
				   :prob (if (funcall call-condition node) 1 0)
				   :left (- left 1))
			   (symbol :discount-rate discount-rate
				   :payoff payment
				   :prob (if (funcall call-condition node) 0 1)
				   :left (- left 1))))
    (create-node chance-node-nucleus-symbol :discount-rate rate :left n)))
