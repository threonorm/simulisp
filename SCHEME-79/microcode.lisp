;;; Global conventions
;; single asterisks (e.g. *nil*): machine registers
;; ampersand (&<whatever>): hardware operation

;;;;;;; Part I: definitions ;;;;;;;

;;; Pointer types
;; Numbers are in octal ?

(defschip **pointer-types**
  '((self-evaluating-pointer 0)
    (symbol 1)
    (global 2)
    (set-global 3)
    (conditional 4)
    (procedure 5)
    (first-argument 6)
    (next-argument 7)
    (last-argument 10)
    (apply-no-args 11)
    (apply-1-arg 12)
    (primitive-apply-1 13)
    (primitive-apply-2 14)
    (sequence 15)
    (spread-argument 16)
    (closure 17)
    (get-control-point 20)
    (control-point 21)
    (interrupt-point 22)
    (self-evaluating-pointer-1 23)
    (self-evaluating-pointer-2 24)
    (self-evaluating-pointer-3 25)
    (self-evaluating-pointer-4 26)
    ))

(defschip **non-pointer-types**
  '((self-evaluating-immediate 100)
    (local 101)
    (tail-local 102)
    (set-local 103)
    (set-tail-local 104)
    (set-only-tail-local 105)
    ; primitives...
    )) ; à compléter

;; 100 bit means non-pointer (??)
(defschip **pointer** 100)

;;; Registers
;; I only include the registers for EVAL for now

;; List of registers

(defschip **machine-registers**
  '((*nil*)
    (*exp*)
    (*args*)
    (*display*)
    (*stack*)
    (*retpc-count-mark*) ; "our registers are not dual rank": ???
    (*intermediate-argument*)))

;; Control wires which enter or exit the registers
(defreg *exp* (to-type to-displacement to-frame from from-decremented) ())
(defreg *val* (to-type to-address from)
              (type=bus address=bus =bus)) ; =bus is AND of type, address=bus
(defreg *retpc-count-mark* (to-type to-address from))
(defreg *stack* (to-type to-address from))
(defreg *args* (to from))
(defreg *display* (to from))
(defreg *intermediate-argument* (to from))
(defreg *nil* (to from))

;; "Additionally, the bus is sensitive to several conditions:"
(defreg bus () (mark-bit type-not-pointer frame=0 displacement=0 address=0)

;; Operations on registers:
;; * fetch, assign are primitives
;; * save, restore are macros

(defmacro save (quantity)
  (assign *stack* (&cons ,quantity (fetch *stack*)))

(defmacro restore (register)
  (progn
    (assign ,register (&car (fetch *stack*)))
    (assign *stack* (&cdr (fetch *stack*)))))


;;;;;;; Part 2: initialisation ;;;;;;;

;;; boot-load: executed by the chip when it starts
;; most of this is useless in the absence of GC

(deftype boot-load
  (assign *scan-up* (fetch *nil*))
  (&increment-scan-up)
  ) ; à compléter...

(deftype done (go-to done))

;;;;;;; Part 3: Mark-and-sweep ;;;;;;;

; not included here

;;;;;;; Part 4: Evaluator ;;;;;;;

;;;;; 4.1. Variable references


(deftype local ; LOCAL<lexical-address>
  (micro-call lookup-exp local-return))
  
(defpc local-return
  (assign *val* (&car (fetch *display*)))
  (dispatch-on-stack))



