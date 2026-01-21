(in-package :financial-analysis.clos)

;;
;; The Meta-Class definitions
;;

;; This is the class of the class
;; When you say (define-frame Bond ...), 'Bond' is an instance of 'frame-class'.
(defclass frame-class (standard-class)
  ())

;; Validation: allow frame-class to inherit from standard-object
(defmethod validate-superclass ((class frame-class) (super standard-class))
  t)

;;
;; Slot definitions
;;

;; Direct slot -- captures what you type in the define-frame macro.
;; We add :range, :units, and :default to the standard slot definition.
(defclass frame-direct-slot-definition (standard-direct-slot-definition)
  ((range     :initarg :range     :reader slot-range     :initform nil)
   (units     :initarg :units     :reader slot-units     :initform nil)
   (default   :initarg :default   :reader slot-default   :initform nil)
   (if-needed :initarg :if-needed :reader slot-if-needed :initform nil)))

;; Effective slot -- what exists at runtime after inheritance is calculated.
(defclass frame-effective-slot-definition (standard-effective-slot-definition)
  ((range     :initarg :range     :reader slot-range     :initform nil)
   (units     :initarg :units     :reader slot-units     :initform nil)
   (default   :initarg :default   :reader slot-default   :initform nil)
   (if-needed :initarg :if-needed :reader slot-if-needed :initform nil)))

;; MOP hooks -- tell CLOS to use our custom slot classes
(defmethod direct-slot-definition-class ((class frame-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'frame-direct-slot-definition))

(defmethod effective-slot-definition-class ((class frame-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'frame-effective-slot-definition))

;; The logic engine
(defvar *computing-slots* nil
  "A list of slots currently being computed to detect circular dependencies.")

(defmethod slot-unbound ((class frame-class) instance slot-name)
  (let* ((slots (class-slots class))
         (slot-def (find slot-name slots :key #'slot-definition-name))
         (daemon (and slot-def (slot-if-needed slot-def))))
    
    (cond
      ;; CASE 1: Circular Reference Detected
      ((member slot-name *computing-slots*)
       (error "Circular dependency detected computing slot: ~a" slot-name))

      ;; CASE 2: We have an If-Needed Daemon
      (daemon
       (let ((*computing-slots* (cons slot-name *computing-slots*))) ; Push to stack
	 (format t "~%[Frame Logic] Triggering daemon for ~a..." slot-name) ;; Debug print
         (let ((result (funcall daemon instance))) ;; Run the logic
           ;; Memoize: Store the result so we don't run this again
           (setf (slot-value instance slot-name) result)
           result)))

      ;; CASE 3: We have a static Default Value
      ((and slot-def (slot-default slot-def))
       (setf (slot-value instance slot-name) (slot-default slot-def)))

      ;; CASE 4: No logic found. Crash as usual.
      (t (call-next-method)))))

;;
;; Inheritance logic
;;

;; This computes the final slot definition from the list of direct slots
;; (the one in the class and the ones in superclasses).
;; Logic: we take the first non-nil value found in the hierarchy.
(defmethod compute-effective-slot-definition ((class frame-class) name direct-slots)
  (let ((effective-slot (call-next-method)))   ;; do the standard work first

    ;; Transfer our custom facets from the Direct slots to the Effective slot
    (setf (slot-value effective-slot 'range)
	  (some #'slot-range direct-slots))

    (setf (slot-value effective-slot 'units)
	  (some #'slot-units direct-slots))

    (setf (slot-value effective-slot 'default)
	  (some #'slot-default direct-slots))

    ;; Transfer the if-needed daemon
    (setf (slot-value effective-slot 'if-needed)
	  (some #'slot-if-needed direct-slots))

    ;; We check if the daemon is a raw list (lambda ...).
    ;; If so, we compile it into a real function.
    (let ((daemon (some #'slot-if-needed direct-slots)))
      (setf (slot-value effective-slot 'if-needed)
            (cond
              ;; Case 1: It's already a compiled function or symbol
              ((or (functionp daemon) (symbolp daemon)) 
               daemon)
              
              ;; Case 2: It's a list starting with LAMBDA -> Compile it!
              ((and (consp daemon) (eq (first daemon) 'lambda))
               (compile nil daemon))
              
              ;; Case 3: Junk or Nil -> pass it through
              (t daemon))))
    
    effective-slot))

;;
;; The "Frame" behavior (daemons & defaults
;;

;; We intercept the setting of the value to check Ranges.
(defmethod (setf slot-value-using-class) :before (new-value (class frame-class) object slot)
  (let ((range (slot-range slot)))
    (when range
      (let ((min (first range))
            (max (second range)))
        ;; Check MIN (if defined)
        (when (and min (< new-value min))
          (error "Value ~a is below minimum ~a for slot ~a" 
                 new-value min (slot-definition-name slot)))
        
        ;; Check MAX (if defined)
        (when (and max (> new-value max))
          (error "Value ~a is above maximum ~a for slot ~a" 
                 new-value max (slot-definition-name slot)))))))
;;
;; The Macro wrapper
;;

;; Syntactic sugar so we don't have to type (:metaclass frame-class) every time
(defmacro define-frame (name superclasses slots &rest options)
  `(defclass ,name ,superclasses
     ,slots
     (:metaclass frame-class)
     ,@options))

