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

;; Skipping all tail-local stuff

(deftype global
  (assign *val*
          (&global-value (fetch *exp*)))
  (dispatch-on-stack))
         
(deftype set-local
  (micro-call lookup-exp set-local-return))

; &set-global-value = &rplaca
(deftype set-global
  (&set-global-value (fetch *exp*) (fetch *val*))
  (dispatch-on-stack))

(defpc lookup-exp
  (if (&frame=0?)
      (progn (assign *display* (&car (fetch *display*)))
             (go-to-count-displacement))
      (progn (&decrement-frame)
             (assign *display* (&cdr (fetch *display*)))
             (go-to lookup-exp)))

(defpc count-displacement
  (if (&displacement=0?)
      (micro-return)
      (progn (&decrement-displacement)
             (assign *display* (&cdr (fetch *display*)))
             (go-to count-displacement))))

;;;;; 4.2. Self-evaluating data

;; multiple types of self-evaluating (immediate|pointer) -> same code

(deftype self-evaluating-immediate
  (assign *val* (fetch *exp*))
  (dispatch-on-stack))

(deftype symbol
  (assign *val* (fetch *exp*))
  (dispatch-on-stack))

(deftype self-evaluating-pointer
  (assign *val* (fetch *exp*))
  (dispatch-on-stack))

;;;;; 4.3. Special operators lambda/if

(deftype procedure
  (assign *val* (&cons (fetch *exp*) (fetch *display*)))
  (&set-type val closure)
  (dispatch-on-stack))

(deftype conditional
  (if (&eq-val (fetch *nil*))
      (assign *exp* (&cdr (fetch *exp*)))
      (assign *exp* (&car (fetch *exp*))))
  (dispatch-on-exp-allowing-interrupts))

;;;;; 4.4. Compound expressions and sequencing

(defmicromacro save-cdr-and-eval-car (return-tag)
  (progn (&set-type *retpc-count-mark* ,return-tag)
         (go-to standard-eval)))

(defpc standard-eval
  (save (fetch *display*))
  (&set-type *stack* (fetch *retpc-count-mark*))
  (save (&cdr (fetch *exp*)))
  (&set-type *stack* standard-return)
  (assign *exp* (&car (fetch *exp*)))
  (dispatch-on-exp-allowing-interrupts))

(defreturn standard-return
  (restore *exp*)
  (assign *retpc-count-mark* (fetch *stack*))
  (restore *display*)
  (dispatch (fetch *retpc-count-mark*)))

(deftype sequence
  (assign *val* (fetch *nil*)) ; for gc
  (save-cdr-and-eval-car sequence-return))

(defreturn sequence-return
  (dispatch-on-exp-allowin-interrupts))

;;;;; 4.5. Control points (continuations?)

; skipping this...

;;;;; 4.6. Procedure calls

(deftype first-argument
  (save-cdr-and-eval-car first-argument-return))

(defreturn first-argument-return
  (assign *args* (&cons (fetch *val*) (fetch *nil*)))
  (save (fetch *args*))
  (dispatch-on-exp-allowing-interrupts))

(deftype next-argument
  (save (fetch *args*))
  (save-cdr-and-eval-car next-argument-return))

(defreturn next-argument-return
  (restore *args*)
  (&rplacd (fetch *args*) (&cons (fetch *val*) (fetch *nil*)))
  (assign *args* (&cdr (fetch *args*)))
  (dispatch-on-exp-allowing-interrupts))

(deftype last-argument
  (save (fetch *args*))
  (save-cdr-and-eval-car last-argument-return))

(defreturn last-argument-return
  (restore *args*)
  (&rplacd (fetch *args*)
           (&cons (fetch *val*) (fetch *nil*)))
  (eval-exp-popj-to internal-apply)) ; Amazing! Where did retpc go?

;; special cases for <= 1 arg

(deftype apply-1-arg
  (save-cdr-and-eval-car apply-1-arg-return))

(defreturn apply-1-arg-return
  (assign *args* (&cons (fetch *val*) (fetch *nil*)))
  (save (fetch *args*))
  (eval-exp-popj-to internal-apply))

(deftype apply-no-args
  (assign *exp* (&car (fetch *exp*)))
  (save (fetch *nil*)) ; ugh! need a place for retpc.
  (eval-exp-popj-to internal-apply))

; skipping spread-argument for now...


;;;;; 4.7. Closures (and continuations)

(deftype closure
  (assign *display*
          (&cons (fetch *args*) (&cdr (fetch *exp*))))
  (assign *exp* (&car (&car (fetch *exp*))))
  (dispatch-on-exp-allowing-interrupts))

; skipping control-point and interrupt-point

;;;;; 4.8. Primitives

; à compléter...  


