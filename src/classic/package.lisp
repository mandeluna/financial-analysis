;;; src/classic/package.lisp

(defpackage :financial-analysis.classic
  (:use :cl)
  (:export 
   #:present-value
   #:net-present-value
   #:internal-rate-of-return
   #:bond-price
   #:macaulay-duration
   #:create-node
   #:create-bond
   #:build-bond-tree
   #:tree-value)) 
