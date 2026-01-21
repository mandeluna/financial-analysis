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
