;; Chapter 11: Welcome Back to the Show

(load "test-check.scm")

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

;; one?: [number] -> boolean
(define one?
  (lambda (n)
    (= n 1)))

(test "one?" (one? 1) #t)
(test "one?" (one? 3) #f)

;; member?: [atom] [lat] -> boolean
;; Page 3
(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
               (member? a (cdr lat)))))))

(test "member?" (member? 'x '(a b c)) #f)
(test "member?" (member? 'x '(a x c)) #t)
(test "member?" (member? 'x '(x b c)) #t)
(test "member?" (member? 'x '(a b x)) #t)
(test "member?" (member? 'b '(a (b) c)) #f)
(test "member?" (member? 'x '()) #f)

;; is-first?: [atom] [lat] -> boolean
;; Page 5
(define is-first?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (eq? (car lat) a)))))

(test "is-first?" (is-first? 'x '(x b c)) #t)
(test "is-first?" (is-first? 'x '(b x c)) #f)
(test "is-first?" (is-first? 'x '(a b c)) #f)

;; two-in-a-row?: [lat] -> boolean
;; Page 5
(define two-in-a-row?
  (lambda (lat)
    (cond
     ((null? lat) #f)
     (else (or (is-first? (car lat) (cdr lat))
               (two-in-a-row? (cdr lat)))))))

(test "two-in-a-row?" (two-in-a-row? '(a b b c)) #t)
(test "two-in-a-row?" (two-in-a-row? '(a b c c)) #t)
(test "two-in-a-row?" (two-in-a-row? '(a a b c)) #t)
(test "two-in-a-row?" (two-in-a-row? '(a b c d)) #f)
(test "two-in-a-row?" (two-in-a-row? '()) #f)

;; is-first-b?: [atom] [lat] -> boolean
;; Page 6
(define is-first-b?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
               (two-in-a-row? lat))))))

(test "is-first-b?" (is-first-b? 'x '(x a b)) #t)
(test "is-first-b?" (is-first-b? 'x '(a b c)) #f)
(test "is-first-b?" (is-first-b? 'x '(a b b c)) #t)

;; 1. Revision of two-in-a-row?
;; two-in-a-row?: [lat] -> boolean
;; Page 6
(define two-in-a-row?
  (lambda (lat)
    (cond
     ((null? lat) #f)
     (else (is-first-b? (car lat) (cdr lat))))))

(test "two-in-a-row? - 1.revision" (two-in-a-row? '(a b b c)) #t)
(test "two-in-a-row? - 1.revision" (two-in-a-row? '(a b c c)) #t)
(test "two-in-a-row? - 1.revision" (two-in-a-row? '(a a b c)) #t)
(test "two-in-a-row? - 1.revision" (two-in-a-row? '(a b c d)) #f)
(test "two-in-a-row? - 1.revision" (two-in-a-row? '()) #f)

;; two-in-a-row-b?: [atom] [lat] -> boolean
;; Page 7
(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) preceding)
               (two-in-a-row-b? (car lat) (cdr lat)))))))

(test "two-in-a-row-b?" (two-in-a-row? '(a b b c)) #t)
(test "two-in-a-row-b?" (two-in-a-row? '(a b c c)) #t)
(test "two-in-a-row-b?" (two-in-a-row? '(a a b c)) #t)
(test "two-in-a-row-b?" (two-in-a-row? '(a b c d)) #f)
(test "two-in-a-row-b?" (two-in-a-row? '()) #f)

;; 2.Revision of two-in-a-row?
;; two-in-a-row?: [lat] -> boolean
;; Page 7
(define two-in-a-row?
  (lambda (lat)
    (cond
     ((null? lat) #f)
     (else (two-in-a-row-b? (car lat) (cdr lat))))))

(test "two-in-a-row? - 2.revision" (two-in-a-row? '(a b b c)) #t)
(test "two-in-a-row? - 2.revision" (two-in-a-row? '(a b c c)) #t)
(test "two-in-a-row? - 2.revision" (two-in-a-row? '(a a b c)) #t)
(test "two-in-a-row? - 2.revision" (two-in-a-row? '(a b c d)) #f)
(test "two-in-a-row? - 2.revision" (two-in-a-row? '()) #f)

;; sum-of-prefixes-b: [number] [tup] -> [number]
;; Page 10
(define sum-of-prefixes-b
  (lambda (sonssf tup)
    (cond
     ((null? tup) '())
     (else (cons (+ sonssf (car tup))
             (sum-of-prefixes-b (+ sonssf (car tup)) (cdr tup)))))))

(test "sum-of-prefixes-b" (sum-of-prefixes-b 2 '(1 2 3 4)) '(3 5 8 12))
(test "sum-of-prefixes-b" (sum-of-prefixes-b 0 '(1 2 3 4)) '(1 3 6 10))
(test "sum-of-prefixes-b" (sum-of-prefixes-b 0 '()) '())

;; sum-of-prefixes: [tup] -> [number]
;; Page 11
(define sum-of-prefixes
  (lambda (tup)
    (sum-of-prefixes-b 0 tup)))

(test "sum-of-prefixes" (sum-of-prefixes '(1 2 3 4)) '(1 3 6 10))
(test "sum-of-prefixes" (sum-of-prefixes '(3 5 7)) '(3 8 15))

;; pick: [number] [lat] -> [number]
;; Page 13
(define pick
  (lambda (n lat)
    (cond
     ((one? n) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(test "pick" (pick 2 '(a b c d e)) 'b)
(test "pick" (pick 4 '(a b c d e)) 'd)
(test "pick" (pick 1 '(a b c d e)) 'a)
(test "pick" (pick 5 '(a b c d e)) 'e)

;; Not allowed -> returns error
;; (test "pick" (pick 6 '(a b c d e)) '())
;; Returns also error
;; (test "pick" (pick 0 '(a b c d e)) '())

;; scramble-b: [tup] [number] -> [tup]
;; Page 14
(define scramble-b
  (lambda (tup rev-pre)
    (cond
     ((null? tup) '())
     (else
      (cons (pick (car tup) (cons (car tup) rev-pre))
        (scramble-b (cdr tup) (cons (car tup) rev-pre)))))))

(test "scramble-b"
      (scramble-b '(1 1 1 3 4 2 1 1 9 2) '())
      '(1 1 1 1 1 4 1 1 1 9))

;; scramble: [tup] -> [tup]
;; Page 15
;;
;; Examples:
;; (scramble '(1 1 1 3 4 2 1 1 9 2))
;; -> (1 1 1 1 1 4 1 1 1 9)
;; (scramble '(1 2 3 1 2 3 4 1 8 2 10))
;; -> (1 1 1 1 1 1 1 1 2 8 2)
(define scramble
  (lambda (tup)
    (scramble-b tup '())))

(test "scramble"
      (scramble '(1 1 1 3 4 2 1 1 9 2))
      '(1 1 1 1 1 4 1 1 1 9))