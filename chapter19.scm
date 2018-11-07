;; Chapter 19: Absconding with the Jewels

(load "test-check.scm")

;; sub1: [number] -> [number]
(define sub1
  (lambda (n)
    (- n 1)))

;; deep: [number] -> [s-exp]
;; Page 155
(define deep
  (lambda (m)
    (if (zero? m)
        'pizza
        (cons (deep (sub1 m)) '()))))

(test "deep" (deep 3) '(((pizza))))
(test "deep" (deep 7) '(((((((pizza))))))))
(test "deep" (deep 0) 'pizza)

;; six-layers: [s-exp] -> list
;; Page 156
(define six-layers
  (lambda (p)
    (cons
        (cons
            (cons
                (cons
                    (cons
                        (cons p '())
                      '())
                  '())
              '())
          '())
      '())))

(test "six-layers"
      (six-layers 'mozzarella)
      '((((((mozzarella)))))))

;; four-layers: [s-exp] -> list
;; Page 157
(define four-layers
  (lambda (p)
    (cons
        (cons
            (cons
                (cons p '())
              '())
          '())
      '())))

(test "four-layers"
      (four-layers 'mozzarella)
      '((((mozzarella)))))

(define toppings #f)

;; deepB: [number] -> list
;; Page 158
(define deepB
  (lambda (m)
    (cond
     ((zero? m) (call/cc
                 (lambda (jump)
                   (set! toppings jump)
                   'pizza)))
     (else (cons (deepB (sub1 m)) '())))))

(test "deepB" (deepB 4) '((((pizza)))))

;; This call is necessary because evaluating (test "deepB" (deepB 4)
;; '((((pizza))))) changes the saved continuation and thus the test of
;; "toppings" below would fail. Comment out (deepB 4) below and try it
;; out yourself and look what the test "toppings" returns. Moreover
;; even with (deepB 4) evaluated the test "toppings" is never
;; evaluated.
(deepB 4)

;; This test would never be evaluated
;; (test "toppings" (toppings 'cake) '((((cake)))))

;; deep&co: [number] cont -> [s-exp]
;; Page 161
(define deep&co
  (lambda (m k)
    (cond
     ((zero? m) (k 'pizza))
     (else
      (deep&co (sub1 m) (lambda (x)
                          (k (cons x '()))))))))

(test "deep&co" (deep&co 0 (lambda (x) x)) 'pizza)
(test "deep&co" (deep&co 6 (lambda (x) x)) '((((((pizza)))))))
(test "deep&co" (deep&co 2 (lambda (x) x)) '((pizza)))

;; two-layers: [s-exp] -> lsit
;; Page 162
(define two-layers
  (lambda (p)
    (cons (cons p '()) '())))

;; deep&coB: [number] cont -> [s-exp]
;; Page 163
(define deep&coB
  (lambda (m k)
    (cond
     ((zero? m) (let ()
                  (set! toppings k)
                  (k 'pizza)))
     (else
      (deep&coB (sub1 m) (lambda (x)
                           (k (cons x '()))))))))

(test "deep&coB" (deep&coB 2 (lambda (x) x)) '((pizza)))
(test "deep&coB" (deep&coB 6 (lambda (x) x)) '((((((pizza)))))))
(test "deep&coB" (deep&coB 4 (lambda (x) x)) '((((pizza)))))
(test "toppings"
      (cons (toppings 'cake) (toppings 'cake))
      '(((((cake)))) (((cake)))))

(test "toppings"
      (cons (toppings 'cake) (cons (toppings 'mozzarella) (cons (toppings 'pizza) '())))
      '(((((cake)))) ((((mozzarella)))) ((((pizza))))))

;; two-in-a-row?: [lat] -> boolean
;; Page 165
(define two-in-a-row?
  (lambda (lat)
    (cond
     ((null? lat) #f)
     (else (two-in-a-row-b? (car lat) (cdr lat))))))

;; two-in-a-row?: [atom] [lat] -> boolean
;; Page 165
(define two-in-a-row-b?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
               (two-in-a-row-b? (car lat) (cdr lat)))))))

(test "tow-in-a-row-b?" (two-in-a-row-b? 'x '(a b x x c e)) #t)
(test "tow-in-a-row-b?" (two-in-a-row-b? 'x '(a b x c e)) #f)
(test "two-in-a-row?" (two-in-a-row? '(a b c c d)) #t)
(test "two-in-a-row?" (two-in-a-row? '(a b c d d)) #t)
(test "two-in-a-row?" (two-in-a-row? '(a b c d)) #f)

;; two-in-a-row?: [lat] -> boolean
;; 1.revision
;; Page 165
(define two-in-a-row?
  (letrec
      ((W (lambda (a lat)
            (cond
             ((null? lat) #f)
             (else
              (let ((nxt (car lat)))
                (or (eq? nxt a) (W nxt (cdr lat)))))))))
    (lambda (lat)
      (cond
       ((null? lat) #f)
       (else (W (car lat) (cdr lat)))))))

(test "two-in-a-row? - 1.revision" (two-in-a-row? '(a b c c d)) #t)
(test "two-in-a-row? - 1.revision" (two-in-a-row? '(a b c d d)) #t)
(test "two-in-a-row? - 1.revision" (two-in-a-row? '(a b c d)) #f)

;; atom?: any -> boolean
(define atom?
  (lambda (e)
    (and (not (pair? e)) (not (null? e)))))

(test "atom?" (atom? 'a) #t)
(test "atom?" (atom? 2) #t)
(test "atom?" (atom? -7) #t)
(test "atom?" (atom? '(a)) #f)
(test "atom?" (atom? '(x y)) #f)
(test "atom?" (atom? '(x . y)) #f)
(test "atom?" (atom? "test") #t)
(test "atom?" (atom? #\h) #t)

(define leave #f)

;; walk: list -> void
;; Page 167
(define walk
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (leave (car l)))
     (else
      (let ()
        (walk (car l))
        (walk (cdr l)))))))

;; leftmost: [s-exp] -> [atom]
;; Page 167
(define leftmost
  (lambda (l)
    (call/cc
     (lambda (skip)
       (letrec
           ((lm (lambda (l)
                  (cond
                   ((null? l) '())
                   ((atom? (car l)) (skip (car l)))
                   (else (let ()
                           (lm (car l))
                           (lm (cdr l))))))))
         (lm l))))))

(test "leftmost"
      (leftmost '((potato) (chips ((with) fish) (chips))))
      'potato)

(test "leftmost"
      (leftmost '(((hot) (tuna (and))) cheese))
      'hot)

(test "leftmost"
      (leftmost '(((a)) b (c)))
      'a)

;; start-it: list -> [s-exp]
;; Page 167
(define start-it
  (lambda (l)
    (call/cc
     (lambda (here)
       (set! leave here)
       (walk l)))))

(test "start-it"
      (start-it '((potato) (chips (chips (with))) fish))
      'potato)

(define fill #f)

;; waddle: list -> [s-exp]
;; Page 169
(define waddle
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (let ()
                        (call/cc
                         (lambda (rest)
                           (set! fill rest)
                           (leave (car l))))
                        (waddle (cdr l))))
     (else (let ()
             (waddle (car l))
             (waddle (cdr l)))))))

;; Note that the previously used testing method does not work because
;; call/cc captures also the test thus remembering the test. 

;; start-it2: list -> [s-exp]
(define start-it2
  (lambda (l)
    (call/cc
     (lambda (here)
       (set! leave here)
       (waddle l)))))

;; Try the following expressions in the repl:
(start-it2 '((donuts) (cheerios (cheerios (spaghettios))) donuts))

(fill '())
;; -> cheerios

;; rest1: [s-exp] -> void
;; Page 171
(define rest1
  (lambda (x)
    (waddle '(() (cheerios (cheerios (spaghettios))) donuts))))

;; get-next: [s-exp] -> void
;; Page 171
(define get-next
  (lambda (x)
    (call/cc
     (lambda (here-again)
       (set! leave here-again)
       (fill 'go)))))

(get-next 'go)
;; -> cheerios

;; rest2: [s-exp] -> void
;; Page 172
(define rest2
  (lambda (x)
    (waddle '(((cheerios (spaghettios))) donuts))))

(get-next 'go)
;; -> cheerios

(get-next 'go)
;; -> spaghettios

(get-next 'go)
;; -> donuts

(get-next 'go)
;; -> '()

(get-next 'go)
;; -> '()

;; get-first: list -> void
;; Page 174
(define get-first
  (lambda (l)
    (call/cc
     (lambda (here)
       (set! leave here)
       (waddle l)
       (leave '())))))

(get-first '(donut))
;; -> donut

(get-next 'go)
;; -> ()

(get-first '(fish (chips)))
;; -> fish

(get-next 'go)
;; -> chips

(get-next 'go)
;; -> ()

(get-first '(fish (chips) chips))
;; -> fish

(get-next 'go)
;; -> chips

(get-next 'go)
;; -> chips

(get-next 'go)
;; -> ()

;; two-in-a-row*?: list -> boolean
;; Page 175
(define two-in-a-row*?
  (lambda (l)
    (let ((fst (get-first l)))
      (if (atom? fst)
          (two-in-a-row-b*? fst)
          #f))))

;; two-in-a-row-b*?: [atom] -> boolean
;; Page 175
(define two-in-a-row-b*?
  (lambda (a)
    (let ((n (get-next 'go)))
      (if (atom? n)
          (or (eq? n a) (two-in-a-row-b*? n))
          #f))))

(test "two-in-a-row*?" (two-in-a-row*? '(a b c c d)) #t)
(test "two-in-a-row*?" (two-in-a-row*? '(a b c d d)) #t)
(test "two-in-a-row*?" (two-in-a-row*? '(a b c d)) #f)
(test "two-in-a-row*?" (two-in-a-row*? '(a b (c) c d)) #t)
(test "two-in-a-row*?" (two-in-a-row*? '(a b (c (d)) d)) #t)
(test "two-in-a-row*?" (two-in-a-row*? '(a b c d)) #f)

;; two-in-a-row*?: list -> boolean
;; 1.revision
;; Page 176
(define two-in-a-row*?
  (letrec
      ((T? (lambda (a)
             (let ((n (get-next 0)))
               (if (atom? n)
                   (or (eq? n a) (T? n))
                   #f))))
       (get-next (lambda (x)
                   (call/cc
                    (lambda (here-again)
                      (set! leave here-again)
                      (fill 'go)))))
       (fill (lambda (x) x))
       (waddle (lambda (l)
                 (cond
                  ((null? l) '())
                  ((atom? (car l)) (let ()
                                     (call/cc
                                      (lambda (rest)
                                        (set! fill rest)
                                        (leave (car l))))
                                     (waddle (cdr l))))
                  (else (let ()
                          (waddle (car l))
                          (waddle (cdr l)))))))
       (leave (lambda (x) x)))
    (lambda (l)
      (let ((fst (call/cc
                  (lambda (here)
                    (set! leave here)
                    (waddle l)
                    (leave '())))))
        (if (atom? fst)
            (T? fst)
            #f)))))

(test "two-in-a-row*? - 1.revision" (two-in-a-row*? '(a b c c d)) #t)
(test "two-in-a-row*? - 1.revision" (two-in-a-row*? '(a b c d d)) #t)
(test "two-in-a-row*? - 1.revision" (two-in-a-row*? '(a b c d)) #f)
(test "two-in-a-row*? - 1.revision" (two-in-a-row*? '(a b (c) c d)) #t)
(test "two-in-a-row*? - 1.revision" (two-in-a-row*? '(a b (c (d)) d)) #t)
(test "two-in-a-row*? - 1.revision" (two-in-a-row*? '(a b c d)) #f)
