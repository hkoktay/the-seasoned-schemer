;; Chapter 20: What's in Store?

;; Note that we test most of the procedures at the end this time
;; because the procedures in this chapter have unbound identifiers the
;; first time we define them.

(load "test-check.scm")

;; Not part of The Seasonend Schemer. This is to make sure the file properly
;; loads and runs the tests of chapter20.scm.
(define ... #f)

;; A [name] is a
;; - symbol
;;
;; A [value] is a
;; - [s-exp]
;;
;; A [table] is 
;; - a function with [name] as argument
;;

;; the-empty-table: [name] -> [table]
;; Page 179
(define the-empty-table
  (lambda (name)
    ...))

;; lookup: [table] [name] -> [s-exp]
;; Page 179
(define lookup
  (lambda (table name)
    (table name)))

(test "lookup" (lookup (lambda (x) x) 'test) 'test)
(test "lookup" (lookup (lambda (x) (cons x '(b))) 'test) '(test b))

;; extend: [name] [value] [table] -> [table]
;; Page 179
(define extend
  (lambda (name1 value table)
    (lambda (name2)
      (cond
       ((eq? name2 name1) value)
       (else (table name2))))))

(test "extend"
      ((extend 'test 1 (lambda (x) (cons x '()))) 'test)
      1)

(test "extend"
      ((extend 'test 1 (lambda (x) (cons x '()))) 'pizza)
      '(pizza))

(define x 3)

;; value: [s-exp] -> [s-exp]
;; Page 180
(define value
  (lambda (e)
    ...
    (cond
     ((define? e) (*define e))
     (else (the-meaning e)))
    ...))

;; atom?: any -> boolean
(define atom?
  (lambda (e)
    (and (not (pair? e)) (not (null? e)))))

;; define?: [s-exp] -> boolean
;; Page 180
(define define?
  (lambda (e)
    (cond
     ((atom? e) #f)
     ((atom? (car e)) (eq? (car e) 'define))
     (else #f))))

(test "define?" (define? '(test)) #f)
(test "define?" (define? '(define)) #t)
(test "define?" (define? '(lambda (x) x)) #f)

(define global-table the-empty-table)

;; *define: [s-exp] -> [table]
;; Page 181
(define *define
  (lambda (e)
    (set! global-table
          (extend (name-of e)
                  (box (the-meaning (right-side-of e)))
                  global-table))))

;; box: [s-exp] -> [box]
;; Page 181
(define box
  (lambda (it)
    (lambda (sel)
      (sel it (lambda (new)
                (set! it new))))))

;; setbox: [box] [s-exp] -> void
;; Page 182
(define setbox
  (lambda (box new)
    (box (lambda (it set) (set new)))))

;; unbox: [box] -> [s-exp]
;; Page 182
(define unbox
  (lambda (box)
    (box (lambda (it set) it))))

(test "box and unbox"
      (let ((one #f))
        (unbox (box one)))
      '#f)

(test "box and unbox"
      (let ((one 10))
        (unbox (box one)))
      '10)

(test "box, setbox and unbox"
      (let ((one (box 10)))
        (setbox one 1)
        (unbox one))
      '1)

;; the-meaning: [s-exp] -> fun
;; Page 182
(define the-meaning
  (lambda (e)
    (meaning e lookup-in-global-table)))

;; lookup-in-global-table: [name] -> [s-exp]
;; Page 182
(define lookup-in-global-table
  (lambda (name)
    (lookup global-table name)))

;; meaning: [s-exp] [table] -> fun
;; Page 183
(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

;; *quote: [s-exp] [table] -> [s-exp]
;; Page 183
(define *quote
  (lambda (e table)
    (text-of e)))

;; *identifier: [s-exp] [table] -> [s-exp]
;; Page 183
(define *identifier
  (lambda (e table)
    (unbox (lookup table e))))

(set! x 5)

(test "x" x 5)

;; *set: [s-exp] [table] -> void
;; Page 184
(define *set
  (lambda (e table)
    (setbox (lookup table (name-of e))
            (meaning (right-side-of e) table))))

;; Page 185
(test "lambda with set!"
      ((lambda (y) (set! x 7) y) 0)
      0)

(test "x" x 7)

;; *lambda: [s-exp] [table] -> fun
;; Page 185
(define *lambda
  (lambda (e table)
    (lambda (args)
      (beglis (body-of e)
              (multi-extend (formals-of e)
                            (box-all args)
                            table)))))

;; beglis: list [table] -> void
;; Page 186
(define beglis
  (lambda (es table)
    (cond
     ((null? (cdr es)) (meaning (car es) table))
     (else ((lambda (val)
              (beglis (cdr es) table))
            (meaning (car es) table))))))

;; box-all: list -> list
;; Page 186
(define box-all
  (lambda (vals)
    (cond
     ((null? vals) '())
     (else (cons (box (car vals))
             (box-all (cdr vals)))))))

;; multi-extend: list list [table] -> [table]
;; Page 187
(define multi-extend
  (lambda (names values table)
    (cond
     ((null? names) table)
     (else
      (extend
       (car names)
       (car values)
       (multi-extend
        (cdr names)
        (cdr values)
        table))))))

;; odd?: [number] -> bool;; odd?: [number] -> boolean
;; Page 188
(define odd?
  (lambda (n)
    (cond
     ((zero? n) #f)
     (else (even? (sub1 n))))))

;; even?: [number] -> boolean
;; Page 188
(define even?
  (lambda (n)
    (cond
     ((zero? n) #f)
     (else (odd? (sub1 n))))))

;; *application: fun [table] -> 
;; Page 190
(define *application
  (lambda (e table)
    ((meaning (function-of e) table)
     (evlis (arguments-of e) table))))

;; evlis: list [table] -> list
;; Page 190
(define evlis
  (lambda (args table)
    (cond
     ((null? args) '())
     (else
      ((lambda (val)
         (cons val
           (evlis (cdr args) table)))
       (meaning (car args) table))))))

;; :car: list -> [s-exp]
;; Page 191
(define :car
  (lambda (args-in-a-list)
    (car (car args-in-a-list))))

;; a-prim: [s-exp] -> fun
;; Page 191
(define a-prim
  (lambda (p)
    (lambda (args-in-a-list)
      (p (car args-in-a-list)))))

;; b-prim: [s-exp] -> fun
;; Page 191
(define b-prim
  (lambda (p)
    (lambda (args-in-a-list)
      (p (car args-in-a-list)
         (car (cdr args-in-a-list))))))

;; *const: [s-exp] [table] -> fun
(define *const
  (lambda (e table)
    (cond
     ((number? e) e)
     ((eq? e #t) #t)
     ((eq? e #f) #f)
     ((eq? e 'cons) (b-prim cons))
     ((eq? e 'car) (a-prim car))
     ((eq? e 'cdr) (a-prim cdr))
     ((eq? e 'eq?) (b-prim eq?))
     ((eq? e 'atom?) (a-prim atom?))
     ((eq? e 'null?) (a-prim null?))
     ((eq? e 'zero?) (a-prim zero?))
     ((eq? e 'add1) (a-prim add1))
     ((eq? e 'sub1) (a-prim sub1))
     ((eq? e 'number?) (a-prim number?)))))

;; add1: [number] -> [number]
(define add1
  (lambda (n)
    (+ n 1)))

(test "add1" (add1 67) 68)
(test "add1" (add1 0) 1)
(test "add1" (add1 -4) -3)

;; sub1: [number] -> [number]
(define sub1
  (lambda (n)
    (- n 1)))

(test "sub1" (sub1 10) 9)
(test "sub1" (sub1 -4) -5)

;; *const: [s-exp] [table] -> fun
;; 1.revision
;; Page 194
(define *const
  (let ((:cons (b-prim cons))
        (:car (a-prim car))
        (:cdr (a-prim cdr))
        (:null? (a-prim null?))
        (:eq? (b-prim eq?))
        (:atom? (a-prim atom?))
        (:zero? (a-prim zero?))
        (:add1 (a-prim add1))
        (:sub1 (a-prim sub1))
        (:number? (a-prim number?)))
    (lambda (e table)
      (cond
       ((number? e) e)
       ((eq? e #t) #t)
       ((eq? e #f) #f)
       ((eq? e 'cons) :cons)
       ((eq? e 'car) :car)
       ((eq? e 'cdr) :cdr)
       ((eq? e 'eq?) :eq?)
       ((eq? e 'atom?) :atom?)
       ((eq? e 'null?) :null?)
       ((eq? e 'zero?) :zero?)
       ((eq? e 'add1) :add1)
       ((eq? e 'sub1) :sub1)
       ((eq? e 'number?) :number?)))))

;; *const: [s-exp] [table] -> fun
;; 2.revision
;; Page 194
(define *const
  ((lambda (:cons
            :car
            :cdr
            :null?
            :eq?
            :atom?
            :zero?
            :add1
            :sub1
            :number?)
     (lambda (e table)
      (cond
       ((number? e) e)
       ((eq? e #t) #t)
       ((eq? e #f) #f)
       ((eq? e 'cons) :cons)
       ((eq? e 'car) :car)
       ((eq? e 'cdr) :cdr)
       ((eq? e 'eq?) :eq?)
       ((eq? e 'atom?) :atom?)
       ((eq? e 'null?) :null?)
       ((eq? e 'zero?) :zero?)
       ((eq? e 'add1) :add1)
       ((eq? e 'sub1) :sub1)
       ((eq? e 'number?) :number?))))
   (b-prim cons)
   (a-prim car)
   (a-prim cdr)
   (a-prim null?)
   (b-prim eq?)
   (a-prim atom?)
   (a-prim zero?)
   (a-prim add1)
   (a-prim sub1)
   (a-prim number?)))

;; *cond: [s-exp] [table] ->
;; Page 195
(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

;; evcon: list [table] ->
;; Page 195
(define evcon
  (lambda (lines table)
    (cond
     ((else? (question-of (car lines)))
      (meaning (answer-of (car lines)) table))
     ((meaning (question-of (car lines)) table)
      (meaning (answer-of (car lines)) table))
     (else (evcon (cdr lines) table)))))

;; *letcc: [s-exp] [table] ->
;; Page 197
(define *letcc
  (lambda (e table)
    (call/cc
     (lambda (skip)
       (beglis (ccbody-of e)
               (extend
                (name-of e)
                (box (a-prim skip))
                table))))))

;; value: [s-exp] -> [s-exp]
;; 1.revision
;; Page 198
(define value
  (lambda (e)
    (call/cc
     (lambda (the-end)
       ...
       (cond
        ((define? e) (*define e))
        (else (the-meaning e)))))))

(define abort #f)

;; value: [s-exp] -> [s-exp]
;; 1.revision
;; Page 198
(define value
  (lambda (e)
    (call/cc
     (lambda (the-end)
       (set! abort the-end)
       (cond
        ((define? e) (*define e))
        (else (the-meaning e)))))))

;; the-empty-table: [name] ->
;; 1.revision
;; Page 199
(define the-empty-table
  (lambda (name)
    (abort
     (cons 'no-anwer
       (cons name '())))))

;; expression-to-action: [s-exp] ->
;; Page 199
(define expression-to-action
  (lambda (e)
    (cond
     ((atom? e) (atom-to-action e))
     (else (list-to-action e)))))

;; atom-to-action: [atom] ->
;; Page 199
(define atom-to-action
  (lambda (e)
    (cond
     ((number? e) *const)
     ((eq? e #t) *const)
     ((eq? e #f) *const)
     ((eq? e 'cons) *const)
     ((eq? e 'car) *const)
     ((eq? e 'cdr) *const)
     ((eq? e 'null?) *const)
     ((eq? e 'eq?) *const)
     ((eq? e 'atom?) *const)
     ((eq? e 'zero?) *const)
     ((eq? e 'add1) *const)
     ((eq? e 'sub1) *const)
     ((eq? e 'number?) *const)
     (else *identifier))))

;; list-to-action: list ->
;; Page 199
(define list-to-action
  (lambda (e)
    (cond
     ((atom? (car e)) (cond
                       ((eq? (car e) 'quote) *quote)
                       ((eq? (car e) 'lambda) *lambda)
                       ((eq? (car e) 'letcc) *letcc)
                       ((eq? (car e) 'set!) *set)
                       ((eq? (car e) 'cond) *cond)
                       (else *application)))
     (else *application))))

;; text-of: list ->
;; Page 200
(define text-of
  (lambda (x)
    (car (cdr x))))

;; formals-of: list ->
;; Page 200
(define formals-of
  (lambda (x)
    (car (cdr x))))

;; body-of: list ->
;; Page 200
(define body-of
  (lambda (x)
    (cdr (cdr x))))

;; ccbody-of: list ->
;; Page 200
(define ccbody-of
  (lambda (x)
    (cdr (cdr x))))

;; name-of: list ->
;; Page 200
(define name-of
  (lambda (x)
    (car (cdr x))))

;; right-side-of: list ->
;; Page 200
(define right-side-of
  (lambda (x)
    (cond
     ((null? (cdr (cdr x))) 0)
     (else (car (cdr (cdr x)))))))

;; cond-lines-of: list ->
;; Page 200
(define cond-lines-of
  (lambda (x)
    (cdr x)))

;; else?: [s-exp] -> boolean
;; Page 200
(define else?
  (lambda (x)
    (cond
     ((atom? x) (eq? x 'else))
     (else #f))))

;; question-of: list -> [s-exp]
;; Page 200
(define question-of
  (lambda (x)
    (car x)))

;; answer-of: list -> [s-exp]
;; Page 200
(define answer-of
  (lambda (x)
    (car (cdr x))))

;; function-of: list -> [s-exp]}
;; Page 200
(define function-of
  (lambda (x)
    (car x)))

;; arguments-of: list -> [s-exp]
(define arguments-of
  (lambda (x)
    (cdr x)))

;; TODO: Write more tests
(test "evlis" (evlis (cons 2 (quote (3))) global-table) '(2 3))
(test "value" (value 1) 1)
(test "value" (value (add1 2)) 3)
(test "value" (value ((lambda (x) (add1 x)) 10)) 11)
(test "value" (value ((lambda (x) (add1 x)) 10)) 11)
(test "value" (value (car (quote (1 2)))) 1)
(test "value" (value (eq? (quote a) (quote a))) #t)

