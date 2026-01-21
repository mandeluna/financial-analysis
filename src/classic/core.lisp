(in-package :financial-analysis.classic)

;;; Ross Miller's "Classic" Financial Analysis
;;; Using standard functions and lists (pre-CLOS style)

(defun present-value (cash-flow discount-rate period)
  "Calculates the PV of a single cash flow at a specific period."
  (/ cash-flow (expt (+ 1.0 discount-rate) period)))

(defun net-present-value (cash-flows discount-rate)
  "Calculates NPV for a list of cash flows assuming equal periods (0, 1, 2...)."
  (let ((period 0))
    (reduce #'+ 
            (mapcar (lambda (cf) 
                      (prog1 
                        (present-value cf discount-rate period)
                        (incf period)))
                    cash-flows))))