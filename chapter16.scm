;; Chapter16: Ready, Set, Bang!

(load "test-check.scm")

;; atom: [any] -> boolean
(define atom?
  (lambda (e)
    (and (not (pair? e)) (not (list? e)))))

;; sub1: [number] -> [number]
(define sub1
  (lambda (n)
    (- n 1)))

;; max: [number] [number] -> [number]
(define max
  (lambda (n m)
    (if (> n m) n m)))

(test "max" (max 2 3) 3)
(test "max" (max 4 2) 4)
(test "max" (max 1 1) 1)

;; member?: [lat] [atom] -> boolean
;; 
;; This time we use equal? instead of eq? because we compare not only
;; symbols but also numbers.
;; Page 29
(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     ((equal? (car lat) a) #t)
     (else (member? a (cdr lat))))))

(test "member?" (member? 'x '(a b c)) #f)
(test "member?" (member? 'x '(a x c)) #t)
(test "member?" (member? 'x '(x b c)) #t)
(test "member?" (member? 'x '(a b x)) #t)
(test "member?" (member? 'b '(a (b) c)) #f)
(test "member?" (member? 'x '()) #f)

;; sweet-tooth: any -> [pair]
;; Page 107
(define sweet-tooth
  (lambda (food)
    (cons food
      (cons 'cake '()))))

(test "sweet-tooth" (sweet-tooth 'chocolate) '(chocolate cake))
(test "sweet-tooth" (sweet-tooth 'fruit) '(fruit cake))

(define last 'angelfood)

(test "last" last 'angelfood)

;; sweet-toothL: any -> [pair]
;; Page 107
(define sweet-toothL
  (lambda (food)
    (set! last food)
    (cons food
      (cons 'cake '()))))

(test "sweet-tooth" (sweet-toothL 'chocolate) '(chocolate cake))
(test "last" last 'chocolate)

(test "sweet-tooth" (sweet-toothL 'fruit) '(fruit cake))
(test "last" last 'fruit)

(test "sweet-tooth" (sweet-toothL 'carrot) '(carrot cake))
(test "last" last 'carrot)

;; Page 108
(define ingredients '())

(test "ingredients" ingredients '())

;; sweet-toothR: any -> [pair]
;; Page 109
(define sweet-toothR
  (lambda (food)
    (set! ingredients (cons food ingredients))
    (cons food
      (cons 'cake '()))))

(test "sweet-toothR" (sweet-toothR 'chocolate) '(chocolate cake))
(test "ingredients" ingredients '(chocolate))

(test "sweet-toothR" (sweet-toothR 'fruit) '(fruit cake))
(test "ingredients" ingredients '(fruit chocolate))

(test "sweet-toothR" (sweet-toothR 'cheese) '(cheese cake))
(test "ingredients" ingredients '(cheese fruit chocolate))

(test "sweet-toothR" (sweet-toothR 'carrot) '(carrot cake))
(test "ingredients" ingredients '(carrot cheese fruit chocolate))

;; deep: [number] -> [pair]
;; Page 110
(define deep
  (lambda (m)
    (cond
     ((zero? m) 'pizza)
     (else (cons (deep (sub1 m)) '())))))

(test "deep" (deep 3) '(((pizza))))
(test "deep" (deep 7) '(((((((pizza))))))))
(test "deep" (deep 0) 'pizza)

;; Stores the numbers from deepR
;; Page 111
(define Ns '())

(test "Ns" Ns '())

;; deepR: [number] -> [pair]
;; Page 111
(define deepR
  (lambda (n)
    (set! Ns (cons n Ns))
    (deep n)))

(test "deepR" (deepR 3) '(((pizza))))
(test "Ns" Ns '(3))

(test "deepR" (deepR 7) '(((((((pizza))))))))
(test "Ns" Ns '(7 3))

(test "deepR" (deepR 0) 'pizza)
(test "Ns" Ns '(0 7 3))

;; Stores the results of deepR
;; Page 111
(define Rs '())

(test "Rs" Rs '())

;; deepR: [number] -> [pair]
;; 1. Revision of deepR
;; Page 111
(define deepR
  (lambda (n)
    (set! Rs (cons (deep n) Rs))
    (set! Ns (cons n Ns))
    (deep n)))

;; Reset the variable 'Ns'
(set! Ns '())
(test "Ns" Ns '())

(test "deepR - 1.revision" (deepR 3) '(((pizza))))
(test "Ns" Ns '(3))
(test "Rs" Rs '((((pizza)))))

(test "deepR - 1.revision" (deepR 7) '(((((((pizza))))))))
(test "Ns" Ns '(7 3))
(test "Rs" Rs '((((((((pizza))))))) (((pizza)))))

(test "deepR - 1.revision" (deepR 0) 'pizza)
(test "Ns" Ns '(0 7 3))
(test "Rs" Rs '(pizza (((((((pizza))))))) (((pizza)))))

;; deepR: [number] -> [pair]
;; 2. Revision of deepR
;; Page 111
(define deepR
  (lambda (n)
    (let ((result (deep n)))
      (set! Rs (cons result Rs))
      (set! Ns (cons n Ns))
      result)))

;; Reset the variable 'Ns' for testing
(set! Ns '())
(test "Ns" Ns '())

;; Reset the variable 'Rs' for testing
(set! Rs '())
(test "Rs" Ns '())

(test "deepR - 2.revision" (deepR 3) '(((pizza))))
(test "Ns" Ns '(3))
(test "Rs" Rs '((((pizza)))))

(test "deepR - 2.revision" (deepR 5) '(((((pizza))))))
(test "Ns" Ns '(5 3))
(test "Rs" Rs '((((((pizza))))) (((pizza)))))

(test "deepR - 2.revision" (deepR 3) '(((pizza))))
(test "Ns" Ns '(3 5 3))
(test "Rs" Rs '((((pizza))) (((((pizza))))) (((pizza)))))

;; find: [number] list list -> [s-exp]
;; Page 113
(define find
  (lambda (n Ns Rs)
    (letrec
        ((A (lambda (ns rs)
              (cond
               ((= (car ns) n) (car rs))
               (else (A (cdr ns) (cdr rs)))))))
      (A Ns Rs))))

(test "find" (find 3 Ns Rs) '(((pizza))))
(test "find" (find 5 Ns Rs) '(((((pizza))))))

;; deepM: [number] -> [pair]
;; Page 113
(define deepM
  (lambda (n)
    (if (member? n Ns)
        (find n Ns Rs)
        (deepR n))))

(set! Ns '())
(set! Rs '())
(test "deepM" (deepM 3) '(((pizza))))
(test "deepM" (deepM 5) '(((((pizza))))))
(test "deepM" (deepM 7) '(((((((pizza))))))))
(test "Ns" Ns '(7 5 3))
(test "Rs" Rs '((((((((pizza))))))) (((((pizza))))) (((pizza)))))

;; Reset Ns and Rs to previous value
(set! Ns '(3 5 3))
(set! Rs '((((pizza))) (((((pizza))))) (((pizza)))))

(test "Ns" Ns '(3 5 3))
(test "Rs" Rs '((((pizza))) (((((pizza))))) (((pizza)))))

(set! Ns (cdr Ns))
(set! Rs (cdr Rs))

(test "Ns" Ns '(5 3))
(test "Rs" Rs '((((((pizza))))) (((pizza)))))

;; deepM: [number] -> [pair]
;; 1. Revision of deepM
;; Page 114
(define deepM
  (lambda (n)
    (if (member? n Ns)
        (find n Ns Rs)
        (let ((result (deep n)))
          (set! Rs (cons result Rs))
          (set! Ns (cons n Ns))
          result))))

(test "Ns" Ns '(5 3))
(test "Rs" Rs '((((((pizza))))) (((pizza)))))
(test "deepM" (deepM 3) '(((pizza))))
(test "deepM" (deepM 6) '((((((pizza)))))))
(test "Ns" Ns '(6 5 3))
(test "Rs" Rs '(((((((pizza)))))) (((((pizza))))) (((pizza)))))

;; deep: [number] -> [pair]
;; 1. Revision of deep
;; Page 115
(define deep
  (lambda (m)
    (cond
     ((zero? m) 'pizza)
     (else (cons (deepM (sub1 m)) '())))))

(set! Ns '())
(set! Rs '())
(test "deep - 1.revision" (deep 3) '(((pizza))))
(test "Ns" Ns '(2 1 0))
(test "Rs" Rs '(((pizza)) (pizza) pizza))

;; deepM: [number] -> [pair]
;; 2. Revision of deepM
;; Page 116
(define deepM
  (lambda (n)
    (let ((Ns '())
          (Rs '()))
      (if (member? n Ns)
          (find n Ns Rs)
          (let ((result (deep n)))
            (set! Rs (cons result Rs))
            (set! Ns (cons n Ns))
            result)))))                     ; (list result Rs Ns)

(set! Ns '())
(set! Rs '())

;; The local variables Ns and Rs of deepM changed but not the global
;; variables Ns and Rs. If you replace the last expression in deepM:
;; result, with the expression (list result Rs Ns) you can see how the
;; local variables changed.
(test "deepM - 2.revision" (deepM 5) '(((((pizza))))))
(test "Ns" Ns '())
(test "Rs" Rs '())

;; find: [number] list list -> [s-exp] | boolean
;; 1. Revision of find
;; Page 117
(define find
  (lambda (n Ns Rs)
    (letrec
        ((A (lambda (ns rs)
              (cond
               ((null? ns) #f)
               ((= (car ns) n) (car rs))
               (else (A (cdr ns) (cdr rs)))))))
      (A Ns Rs))))

(test "find - 1.revision" (find '() '() '()) #f)

;; deepM: [number] -> [pair]
;; 3. Revision of deepM
;; Page 118
(define deepM
  (lambda (n)
    (let ((Ns '())
          (Rs '()))
      (if (atom? (find n Ns Rs))
          (let ((result (deep n)))
            (set! Rs (cons result Rs))
            (set! Ns (cons n Ns))
            result)
          (find n Ns Rs)))))

(test "Ns" Ns '())
(test "Rs" Rs '())
(test "deepM - 3.revison" (deepM 3) '(((pizza))))
(test "deepM - 3.revison" (deepM 6) '((((((pizza)))))))
(test "deepM - 3.revison" (deepM 3) '(((pizza))))
(test "Ns" Ns '())
(test "Rs" Rs '())

;; deepM: [number] -> [pair]
;; 4. Revision of deepM
;; Page 118
(define deepM
  (let ((Ns '())
        (Rs '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (deep n)))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

(test "Ns" Ns '())
(test "Rs" Rs '())
(test "deepM - 4.revision" (deepM 3) '(((pizza))))
(test "deepM - 4.revision" (deepM 6) '((((((pizza)))))))
(test "deepM - 4.revision" (deepM 3) '(((pizza))))
(test "Ns" Ns '())
(test "Rs" Rs '())

;; deepM: [number] -> [pair]
;; 3. Revision of deepM
;; Page 118
;; (define (deepM n)
;;   (let ((Ns '())
;;         (Rs '()))
;;     (let ((exists (find n Ns Rs)))
;;       (if (atom? exists)
;;           (let ((result (deep n)))
;;             (set! Rs (cons result Rs))
;;             (set! Ns (cons n Ns))
;;             result)
;;           exists))))

;; add1: [number] -> [number]
(define add1
  (lambda (n)
    (+ n 1)))

(test "add1" (add1 67) 68)
(test "add1" (add1 0) 1)
(test "add1" (add1 -4) -3)

;; length: [s-exp] -> [number]
(define length
  (lambda (l)
    (cond
     ((null? l) 0)
     (else (add1 (length (cdr l)))))))

(test "length" (length '(a b c)) 3)
(test "length" (length '(a c)) 2)
(test "length" (length '()) 0)

;; length: [s-exp] -> 0
;; 1. Revision of length
;; Page 119
(define length
  (lambda (l) 0))

(test "length - 1.revision" (length '()) 0)
(test "length - 1.revision" (length '(a b c)) 0)

;; 2. Revision of length
;; Page 119
(set! length
      (lambda (l)
        (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))

(test "length - 2.revision" (length '()) 0)
(test "length - 2.revision" (length '(a b c)) 3)

;; length: [s-exp] -> [number]
;; 3. Revision of length
;; Page 119
(define length
  (let ((h (lambda (l) 0)))
    (set! h
          (lambda (l)
            (cond
             ((null? l) 0)
             (else (add1 (h (cdr l)))))))
    h))

(test "length - 3.revision" (length '()) 0)
(test "length - 3.revision" (length '(a b c)) 3)

;; L: fun -> fun
;; Page 121
(define L
  (lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

(test "L" ((L length) '()) 0)
(test "L" ((L length) '(a b c)) 3)

;; length: [s-exp] -> [number]
;; 4. Revision of length
;; Page 122
(define length
  (let ((h (lambda (l) 0)))
    (set! h (L (lambda (arg) (h arg))))
    h))

(test "length - 4.revision" (length '()) 0)
(test "length - 4.revision" (length '(a b c)) 3)

;; The applicative-order imperative Y combinator
;; Page 123
(define Y!
  (lambda (L)
    (let ((h (lambda (l) '())))
      (set! h (L (lambda (arg) (h arg))))
      h)))

;; length
(test "Y!" ((Y! L) '()) 0)
(test "Y!" ((Y! L) '(a b c)) 3)
(test "Y!" ((Y! L) '(a b c d e)) 5)

;; The applicative-order imperative Y combinator
;; Page 123
(define Y-bang
  (lambda (f)
    (letrec
        ((h (f (lambda (arg) (h arg)))))
      h)))

(test "Y-bang" ((Y! L) '()) 0)
(test "Y-bang" ((Y! L) '(a b c)) 3)
(test "Y-bang" ((Y! L) '(a b c d e)) 5)

;; 5.revision of length
;; length: list -> number
(define length (Y! L))

(test "length - 5.revision" (length '()) 0)
(test "length - 5.revision" (length '(a b c)) 3)

;; Page 124
(define D
  (lambda (depth*)
    (lambda (s)
      (cond
       ((null? s) 1)
       ((atom? (car s)) (depth* (cdr s)))
       (else (max (add1 (depth* (car s)))
                  (depth* (cdr s))))))))

;; depth*
(test "D" ((Y! D) '((pickled) peppers (peppers pickled))) 2)
(test "D" ((Y! D) '(margarine ((bitter butter) (makes) (batter (bitter))) butter)) 4)
(test "D" ((Y! D) '(c (b (a b) a) a)) 3)
(test "D" ((Y! D) '(() ((bitter butter) (makes) (batter (bitter))) butter)) 4)

;; Page 124
;; depth*: list -> number
(define depth* (Y! D))

(test "depth*" (depth* '((pickled) peppers (peppers pickled))) 2)
(test "depth*" (depth* '(margarine ((bitter butter) (makes) (batter (bitter))) butter)) 4)
(test "depth*" (depth* '(c (b (a b) a) a)) 3)
(test "depth*" (depth* '(() ((bitter butter) (makes) (batter (bitter))) butter)) 4)

;; Page 124
(define biz
  (let ((x 0))
    (lambda (f)
      (set! x (add1 x))
      (lambda (a)
        (if (= a x)
            0
            (f a))))))

;; ((Y biz) 5) -> 0
;; Works just one time. Why?
;; 
;; 1. ((Y! biz) 5) is applied.
;; 2. the variable 'x' in biz is set to 5.
;; 3. Reapplying ((Y! biz) 5) again sets 'x' to 6 because of (set! x (add1 x)).
;; 4. 'biz' keeps applying (add1 x) to x
;; 5. Since 'a' in 'biz' is 5 and because of (4.), (= a x) can never be true
