;;; src/clos/package.lisp

(defpackage :financial-analysis.clos
  (:use :cl :closer-mop) ;; Import CL and the Compatibility Layer for MOP

  ;; FIX: Explicitly prefer Closer-MOP definitions over standard CL ones
  (:shadowing-import-from :closer-mop
                          #:defclass
                          #:defgeneric
                          #:defmethod
                          #:standard-class
                          #:standard-generic-function
                          #:standard-method
                          #:standard-slot-definition)
  
  ;; MOP often requires explicit export of the meta-object components
  (:export 
   #:frame-class             ;; The metaclass
   #:frame-object            ;; The root class for all frames
   #:define-frame            ;; The macro replacing defclass
   
   ;; Facet accessors
   #:slot-range
   #:slot-units
   #:slot-default
   
   ;; Domain Objects
   #:asset
   #:security
   #:bond))
