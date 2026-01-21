(in-package :financial-analysis.classic)

;;
;; A Decision Tree is an analytical tool whose value is determined by
;; applying a recursive set of valuation rules to the tree.
;;
;; It is characterized by three different types of nodes:
;;
;; * decision node - has one or more successor nodes from which the
;;   decision maker must make an active choice.
;;
;; * chance node - also known as a probability node, has one or more
;;   successor nodes that are randomly chosen by the external environment.
;;
;; * terminal node - contains the monetory (or other) payoff associated
;;   with reaching that node

(defun create-node (name &rest proplist)
  (setf (symbol-plist name) nil)    ; Clear prop-list
  (create-node-aux name proplist)   ; Assign new values
  name)                             ; Returns name of node

(defun create-node-aux (name proplist)
  (if
   (null proplist)
   nil
   (progn
     (setf (get name (first proplist)) (second proplist))
     (create-node-aux name (rest (rest proplist))))))

(defun node-type (node)
  (get node :type))

(defun node-successors (node)
  (get node :successors))

(defun node-prob (node)
  (get node :prob))

(defun node-payoff (node)
  (get node :payoff))

(defun node-rate (node)
  (get node :rate))

;;
;; A decision tree is evaluated according to the following rules
;;
;; if node-type = 'decision, then (value node) = (maximum (value node-successors))
;; if node-type = 'chance, then (value node) = (expected-value (value node-successors))
;; if node-type = 'terminal, then (value node) = (node-payoff)

(defun tree-value (node) ; Simple decision-tree valuation function
  (let
      ((rate (node-rate node))
       (payoff (node-payoff node))
       (successors (node-successors node)))
    (+
     (if
      payoff   ;; Is a cash flow received at the node?
      payoff   ;; If so, add it to the node value
      0)       ;; Otherwise, add nothing
     (*
      (if
       rate             ;; Is a discount rate present?
       (dfactor rate)   ;; If so, use it to discount
       1)               ;; Otherwise, leave undiscounted
    (cond
      ((terminal node) 0)
      ((decision node) (maximum-value successors))
      ((chance node) (expected-value successors)))))))

(defun dfactor (r)
  (/ 1 (+ 1 r)))

(defun decision (node)
  (equal (node-type node) 'decision))

(defun chance (node)
  (equal (node-type node) 'chance))

(defun terminal (node)
  (or
   (equal (node-type node) 'terminal)
   (term-cond node)))

(defun term-cond (node)
  (null (node-successors node)))

(defun maximum-value (nodes)
  (apply 'max (mapcar 'tree-value nodes)))

(defun expected-value (nodes)
  (expectation (mapcar 'node-prob nodes)
	       (mapcar 'tree-value nodes)))

(defun expectation (prob-list value-list)
  (apply '+ (mapcar '* prob-list value-list)))
