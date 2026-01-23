(in-package :financial-analysis.classic)
;;
;; Chapter 6 - A Lisp Implementation of the Basic Binomial Model
;;
;; Retrieve attributes of a node from its list representation
;; or from properties that appear in its nucleus
;;
(defun my-get (node keyword)                 ; Extension of GET
  (if
   (listp node)
   (get-slot node keyword)                   ; Use GET-SLOT for lists
   (get node keyword)))                      ; Otherwise, use GET

(defun get-slot (node keyword)
  (let
      ((symbol (first node))                 ; Node begins with a symbol
       (target (second node))                ; The target attribute
       (keyword-value (caddr node))          ; CADDR is the same as third
       (restargs (cdddr node)))              ; Everything else
    (or
     (cond                                   ; First, try the list
       ((null target) nil)                   ; Done?
       ((equal keyword target) keyword-value) ; Keyword?
       ('otherwise (get-slot                 ; Recursion step
		    (cons symbol restargs)
		    keyword)))
     (funcall 'get symbol keyword))))        ; Then, try properties

;;
;; Miscellaneous selector functions for nodes
;;
(defun node-type (node)
  (my-get node :type))

(defun node-prob (node)
  (my-get node :prob))

(defun node-rate (node)
  (my-get node :rate))

(defun node-stock-price (node)
  (my-get node :price))

(defun node-successor-nucleus-symbol (node)
  (my-get node :successor-nucleus-symbol))

(defun node-exercise-price (node)
  (my-get node :exercise-price))

(defun node-exercise-function (node)
  (my-get node :exercise-function))

(defun node-left (node)
  (my-get node :left))

(defun node-upprob (node)
  (my-get node :upprob))

(defun node-upamt (node)
  (my-get node :upamt))

(defun node-downamt (node)
  (my-get node :downamt))

(defun node-payoff (node)                    ; Includes exercise function
  (+
   (if
    (my-get node :payoff)
    (my-get node :payoff)
    0)
   (if
    (terminal node)
    (funcall (node-exercise-function node) node)
    0)))

(defun node-successors (node)
  (let
    ((raw-successors (my-get node :successors)))
    (mapcar 'list-eval (translate node raw-successors))))

;; auxilary functions for node-successors
(defun list-eval (list)
  (mapcar 'eval list))

;; use defparameter to define global instead of setf
;; extended for bonds
(defparameter *node-translation-table*
      '((symbol . (node-successor-nucleus-symbol node))
	(price . (node-stock-price node))
	(type . (node-type node))
	(rate . (node-rate node))
	(discount-rate . (node-discount-rate node))
	(payment . (node-payment node))
	(call-condition . (node-call-condition node))
	(left . (node-left node))
	(upprob . (node-upprob node))
	(upamt . (node-upamt node))
	(downamt . (node-downamt node))))

(defun translate (node raw-successor)
  (sublis (list (cons 'node (list 'quote node)))
	  (sublis *node-translation-table* raw-successor)))

(defun propor-change (base change)
  (* base (+ 1.0 change)))

;;
;; Option Frame Constructor
;;
(defun create-option (name &rest proplist)
  (setf (symbol-plist name) nil)
  (create-node-aux name proplist)
  (cond
    ((equal (option-type name) 'call)
     (setf (get name :exercise-function)
	   'call-exercise-function))
    ((equal (option-type name) 'put)
     (setf (get name :exercise-function)
	   'put-exercise-function)))
  name)

;;
;; Option Frame accessors
;;
(defun call-exercise-function (node)
  (let
      ((s (node-stock-price node))
       (k (node-exercise-price node)))
    (max 0 (- s k))))

(defun put-exercise-function (node)
  (let
      ((s (node-stock-price node))
       (k (node-exercise-price node)))
    (max 0 (- k s))))

(defun option-exercise-price (option)
  (get option :exercise-price))

(defun option-stock-price (option)
  (get option :stock-price))

(defun option-time (option)
  (get option :time))

(defun option-rate (option)
  (get option :rate))

(defun option-sd (option)
  (get option :sd))

(defun option-exercise-function (option)
  (get option :exercise-function))

(defun option-type (option)
  (get option :type))

;;
;; Decision Tree valuation
;;
(defun tree-value (node)
  (let
      ((rate (node-rate node))
       (payoff (node-payoff node))
       (successors (node-successors node)))
    (+
     (if
      payoff
      payoff
      0)
     (*
      (if
       rate
       (dfactor rate)
       1)
      (cond
	((terminal node) 0)
	((decision node) (maximum-value successors))
	((chance node)   (expected-value successors)))))))

(defun term-cond (node)                     ; New for options and bonds
  (let
      ((left (node-left node))
       (prob (node-prob node)))
    (or                                     ; Two termination conditions
     (and (numberp left)  (zerop left))     ; LEFT=0
     (and (numberp prob)  (zerop prob)))))  ; PROB=0

(defun create-node (nucleus-symbol &rest proplist)
  (cons nucleus-symbol proplist))

(defun create-node-nucleus (name &rest proplist)
  (setf (symbol-plist name) nil)
  (create-node-aux name proplist)
  name)

(defun create-node-aux (name proplist)
  (if
   (null proplist)
   nil
   (progn
     (setf (get name (first proplist)) (second proplist))
     (create-node-aux name (rest (rest proplist))))))

(defun decision (node)
  (equal (node-type node) 'decision))

(defun chance (node)
  (equal (node-type node) 'chance))

(defun terminal (node)
  (or
   (equal (node-type node) 'terminal)
   (term-cond node)))

(defun dfactor (r)
  (/ 1 (+ 1 r)))

(defun maximum-value (nodes)
  (apply 'max (mapcar 'tree-value nodes)))

(defun expected-value (nodes)
  (expectation (mapcar 'node-prob nodes)
	 (mapcar 'tree-value nodes)))

(defun expectation (prob-list value-list)
  (apply '+ (mapcar '* prob-list value-list)))

;;
;; The frame generated by CREATE-OPTION is converted into the appropriate decision tree
;;
(defun build-binom-tree (option n)
  (let*
    ((node-nucleus-symbol (gensym))
     (s (option-stock-price option))
     (k (option-exercise-price option))
     (exercise-function (option-exercise-function option))
     (time (float (option-time option)))
     (rate (- (expt (+ 1 (option-rate option))
		    (/ time n))
	      1))
     (sd (* (option-sd option)
	    (sqrt (/ time n)))))
    (create-node-nucleus node-nucleus-symbol
			 :type 'chance
			 :rate rate
			 :upprob 0.5
			 :upamt (+ rate sd)
			 :downamt (- rate sd)
			 :exercise-price k
			 :exercise-function exercise-function
			 :successor-nucleus-symbol node-nucleus-symbol
			 :successors
			 '((symbol :price (propor-change price upamt)
				   :prob upprob
				   :left (- left 1))
			   (symbol :price (propor-change price downamt)
				   :prob (- 1 upprob)
				   :left (- left 1))))
    (create-node node-nucleus-symbol :price s :left n)))

;;
;; Return the value of the option from its frame representation and the desired valuation method
;;
(defun project-value
    (project-name model-builder &rest model-parms)
  (tree-value (apply model-builder
		     (cons project-name model-parms))))


;;
;; Extension of binomial model to handle American options
;;
(defun build-amer-binom-tree (option n)
  (let*
      ((decision-node-nucleus-symbol (gensym))
       (chance-node-nucleus-symbol (gensym))
       (s (option-stock-price option))
       (k (option-exercise-price option))
       (exercise-function (option-exercise-function option))
       (time (float (option-time option)))
       (rate (- (expt (+ 1 (option-rate option))
		      (/ time n))
		1))
       (sd (*  (option-sd option)
	       (sqrt (/ time n)))))
    (create-node-nucleus decision-node-nucleus-symbol
			 :type 'decision
			 :exercise-price k
			 :exercise-function exercise-function
			 :successor-nucleus-symbol chance-node-nucleus-symbol
			 :successors
			 '((symbol :price price :left left)
			   (symbol :price price :left 0)))
    (create-node-nucleus chance-node-nucleus-symbol
			 :type 'chance
			 :rate rate          ; Discounting only occurs at chance nodes
			 :upprob 0.5
			 :upamt   (+ rate sd)
			 :downamt (- rate sd)
			 :exercise-price k
			 :exercise-function exercise-function
			 :successor-nucleus-symbol decision-node-nucleus-symbol
			 :successors
			 '((symbol :price (propor-change price upamt)
				   :prob upprob
				   :left (- left 1))
			   (symbol :price (propor-change price downamt)
				   :prob (- 1 upprob)
				   :left (- left 1))))
    (create-node decision-node-nucleus-symbol :price s :left n)))

