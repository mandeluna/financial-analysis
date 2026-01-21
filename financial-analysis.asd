;;; financial-analysis.asd

(asdf:defsystem "financial-analysis"
  :description "Replication of Ross Miller's Computer-Aided Financial Analysis"
  :author "Steven Wart"
  :license "MIT"
  :version "0.0.1"
  :in-order-to ((test-op (test-op "financial-analysis/tests"))))

;;; SYSTEM 1: The "Classic" Version (Miller's Book)
;;; Pure Common Lisp, no external dependencies needed.
(asdf:defsystem "financial-analysis/classic"
  :depends-on ()                ;; No dependencies for standard CL
  :pathname "src/classic/"      ;; Look in this folder
  :serial t                     ;; IMPORTANT: Load files in the exact order listed below
  :components ((:file "package")
               (:file "core")
	       (:file "decision-trees")
               (:file "bonds")))

;;; SYSTEM 2: The "CLOS" Version (MOP Enabled)
;;; Depends on 'closer-mop' to standardize MOP behavior across implementations.
(asdf:defsystem "financial-analysis/clos"
  :depends-on ("closer-mop")    ;; External library for MOP compatibility
  :pathname "src/clos/"
  :serial t
  :components ((:file "package")))
