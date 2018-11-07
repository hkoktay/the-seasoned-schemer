;; Chapter 12: Take Cover

(load "test-check.scm")

;; atom?: any -> boolean
(define atom?
  (lambda (e)
    (and (not (pair? e)) (not (null? e)))))

;; add1: [number] -> [number]
(define add1
  (lambda (n)
    (+ n 1)))

;; The applicative-order Y combinator
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

(test "Y"
      ((Y (lambda (length)
            (lambda (l)
              (cond
               ((null? l) 0)
               (else (add1 (length (cdr l))))))))
       '(a b c))
      3)

(test "Y"
      ((Y (lambda (length)
            (lambda (l)
              (cond
               ((null? l) 0)
               (else (add1 (length (cdr l))))))))
       '(a b c d e f))
      6)

(test "Y"
      ((Y (lambda (leftmost)
            (lambda (l)
              (cond
               ((atom? l) l)
               (else (leftmost (car l)))))))
       '((potato) (chips ((with) fish) (chips))))
      'potato)

;; multirember: [atom] [lat] -> [lat]
;; Page 17
(define multirember
  (lambda (a lat)
    ((Y (lambda (mr)
          (lambda (lat)
            (cond
             ((null? lat) '())
             ((eq? a (car lat))
              (mr (cdr lat)))
             (else (cons (car lat)
                     (mr (cdr lat))))))))
     lat)))

(test "multirember"
      (multirember 'a '())
      '())

(test "multirember"
      (multirember 'x '(x a b x c d x))
      '(a b c d))

(test "multirember"
      (multirember '(a b) '(x y (a b) z (a b)))
      '(x y (a b) z (a b)))

;; length: [lat] -> [number]
;; Page 17
(define length
  ((lambda (le)
     ((lambda (f) (f f))
      (lambda (f)
        (le (lambda (x) ((f f) x))))))
   (lambda (length)
     (lambda (l)
       (cond
        ((null? l) 0)
        (else (add1 (length (cdr l)))))))))

(test "length"
      (length '(a b c)) 3)

(test "length"
      (length '()) 0)

(test "length"
      (length '(a)) 1)

(test "length"
      (length '(a (b c) (d))) 3)

;; length: [lat] -> [number]
;; 1. revision of length
;; Page 17
(define length
  (Y (lambda (length)
       (lambda (l)
         (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))))

(test "length - 1.revision"
      (length '(a b c)) 3)

(test "length - 1.revision"
      (length '()) 0)

(test "length - 1.revision"
      (length '(a)) 1)

(test "length - 1.revision"
      (length '(a (b c) (d))) 3)

;; multirember: [atom] [lat] -> [lat]
;; 1. revision of multirember
;; Page 18
(define multirember
  (lambda (a lat)
    ((letrec
         ((mr (lambda (lat)
                (cond
                 ((null? lat) '())
                 ((eq? a (car lat)) (mr (cdr lat)))
                 (else (cons (car lat)
                         (mr (cdr lat))))))))
       mr)
     lat)))

(test "multirember - 1.revision"
      (multirember 'a '())
      '())

(test "multirember - 1.revision"
      (multirember 'x '(x a b x c d x))
      '(a b c d))

(test "multirember - 1.revision"
      (multirember '(a b) '(x y (a b) z (a b)))
      '(x y (a b) z (a b)))

;; Loading mr from a file would cause an error because the variable a is not bound 
;; mr: [lat] -> [lat]
;; Page 18
;; (define (mr lat)
;;   (cond
;;    ((null? lat) '())
;;    ((eq? a (car lat)) (mr (cdr lat)))
;;    (else (cons (car lat)
;;            (mr (cdr lat))))))

;; multirember: [atom] [lat] -> [lat]
;; 2. revision of multirember
;; 
;; This version does not work. Because the variable 'a' in mr is not
;; bound. It's not bound because it's not the same scope. Compare it
;; with the definition of multirember below.
;; 
;; Page 12
;; multirember: [atom] [lat] -> [lat]
;; Same as this version
(define multirember
  (lambda (a lat)
    (define mr
      (lambda (lat)
        (cond
         ((null? lat) '())
         ((eq? a (car lat)) (mr (cdr lat)))
         (else (cons (car lat)
                 (mr (cdr lat)))))))
    (mr lat)))

(test "multirember - 2.revision"
      (multirember 'a '())
      '())

(test "multirember - 2.revision"
      (multirember 'x '(x a b x c d x))
      '(a b c d))

(test "multirember - 2.revision"
      (multirember '(a b) '(x y (a b) z (a b)))
      '(x y (a b) z (a b)))

;; multirember: [atom] [lat] -> [lat]
;; Page 18
(define multirember
  (lambda (a lat)
    ((letrec
         ((mr (lambda (lat)
                (cond
                 ((null? lat) '())
                 ((eq? a (car lat)) (mr (cdr lat)))
                 (else (cons (car lat)
                         (mr (cdr lat))))))))
       mr)
     lat)))

(test "multirember"
      (multirember 'a '())
      '())

(test "multirember"
      (multirember 'x '(x a b x c d x))
      '(a b c d))

(test "multirember"
      (multirember '(a b) '(x y (a b) z (a b)))
      '(x y (a b) z (a b)))

;; id: any -> any
;; Page 19
(define id
  (lambda (a) a))

;; multirember: [atom] [lat] -> [lat]
;; 3. revision of multirember
;; Page 22
(define multirember
  (lambda (a lat)
    (letrec
        ((mr (lambda (lat)
               (cond
                ((null? lat) '())
                ((eq? a (car lat)) (mr (cdr lat)))
                (else (cons (car lat)
                        (mr (cdr lat))))))))
      (mr lat))))

(test "multirember - 3.revision"
      (multirember 'a '())
      '())

(test "multirember - 3.revision"
      (multirember 'x '(x a b x c d x))
      '(a b c d))

(test "multirember - 3.revision"
      (multirember '(a b) '(x y (a b) z (a b)))
      '(x y (a b) z (a b)))

;; rember-f: fun -> fun
;; Page 23
(define rember-f
  (lambda (test?)
    ;; [atom] list -> list
    (lambda (a l)
      (cond
       ((null? l) '())
       ((test? (car l) a) (cdr l))
       (else (cons (car l)
               ((rember-f test?) a (cdr l))))))))

(test "rember-f"
      ((rember-f =) 5 '(6 5 2 3))
      '(6 2 3))

(test "rember-f"
      ((rember-f eq?) 'jelly '(jelly beans are good))
      '(beans  are good))

(test "rember-f"
      ((rember-f equal?) '(pop corn) '(lemonade (pop corn) and (cake)))
      '(lemonade and (cake)))

;; rember-eq?: [atom] [lat] -> [lat]
;; Page 23
(define rember-eq? (rember-f eq?))

(test "rember-eq?"
      (rember-eq? 'tuna '(tuna salad is good))
      '(salad is good))

(test "rember-eq?"
      (rember-eq? 'x '(a b x c d))
      '(a b c d))

;; multirember-f: fun -> fun
;; Page 23
(define multirember-f
  (lambda (test?)
    ;; [atom] [lat] -> [lat]
    (lambda (a lat)
      (cond
       ((null? lat) '())
       ((test? (car lat) a) ((multirember-f test?) a (cdr lat)))
       (else (cons (car lat) ((multirember-f test?) a (cdr lat))))))))

(test "multirember-f"
      ((multirember-f eq?) 'a '())
      '())

(test "multirember-f"
      ((multirember-f eq?) 'x '(x a b x c d x))
      '(a b c d))

(test "multirember-f"
      ((multirember-f eq?) '(a b) '(x y (a b) z (a b)))
      '(x y (a b) z (a b)))

(test "multirember-f"
      ((multirember-f equal?) '(a b) '(x y (a b) z (a b)))
      '(x y z))

;; multirember-f: fun -> fun
;; 1. revision of multirember-f
;; Page 25
(define multirember-f
  (lambda (test?)
    (letrec
        ;; m-f: [atom] [lat] -> [lat]
        ((m-f (lambda (a lat)
                (cond
                 ((null? lat) '())
                 ((test? a (car lat)) (m-f a (cdr lat)))
                 (else (cons (car lat) (m-f a (cdr lat))))))))
      m-f)))

(test "multirember-f - 1.revision"
      ((multirember-f eq?) 'a '())
      '())

(test "multirember-f - 1.revision"
      ((multirember-f eq?) 'x '(x a b x c d x))
      '(a b c d))

(test "multirember-f - 1.revision"
      ((multirember-f eq?) '(a b) '(x y (a b) z (a b)))
      '(x y (a b) z (a b)))

(test "multirember-f - 1.revision"
      ((multirember-f equal?) '(a b) '(x y (a b) z (a b)))
      '(x y z))

;; multirember: [atom] [lat] -> [lat]
;; 4. revision of multirember
;; Page 25
(define multirember
  (letrec
      ((multirember (lambda (a lat)
                      (cond
                       ((null? lat) '())
                       ((eq? (car lat) a) (multirember a (cdr lat)))
                       (else (cons (car lat) (multirember a (cdr lat))))))))
    multirember))

(test "multirember - 4.revision"
      (multirember 'a '())
      '())

(test "multirember - 4.revision"
      (multirember 'x '(x a b x c d x))
      '(a b c d))

(test "multirember - 4.revision"
      (multirember '(a b) '(x y (a b) z (a b)))
      '(x y (a b) z (a b)))

;; multirember: [atom] [lat] -> [lat]
;; 5. revision of multirember
;; Page 26
(define multirember
  (lambda (a lat)
   (cond
    ((null? lat) '())
    ((eq? (car lat) a) (multirember a (cdr lat)))
    (else (cons (car lat) (multirember a (cdr lat)))))))

(test "multirember - 5.revision"
      (multirember 'a '())
      '())

(test "multirember - 5.revision"
      (multirember 'x '(x a b x c d x))
      '(a b c d))

(test "multirember - 5.revision"
      (multirember '(a b) '(x y (a b) z (a b)))
      '(x y (a b) z (a b)))

;; member?: [atom] [lat] -> boolean
;; Page 27
(define member?
  (lambda (a lat)
    ((letrec
         ((yes? (lambda (l)
                  (cond
                   ((null? l) #f)
                   ((eq? (car l) a) #t)
                   (else (yes? (cdr l)))))))
       yes?)
     lat)))

(test "member?" (member? 'x '(a b c)) #f)
(test "member?" (member? 'x '(a x c)) #t)
(test "member?" (member? 'x '(x b c)) #t)
(test "member?" (member? 'x '(a b x)) #t)
(test "member?" (member? 'b '(a (b) c)) #f)
(test "member?" (member? 'x '()) #f)

;; member?: [atom] [lat] -> boolean
;; 1. revision of member?
;; Page 27
(define member?
  (lambda (a lat)
    (letrec
        ((yes? (lambda (l)
                 (cond
                  ((null? l) #f)
                  ((eq? (car l) a) #t)
                  (else (yes? (cdr l)))))))
      (yes? lat))))

(test "member? - 1.revision" (member? 'x '(a b c)) #f)
(test "member? - 1.revision" (member? 'x '(a x c)) #t)
(test "member? - 1.revision" (member? 'x '(x b c)) #t)
(test "member? - 1.revision" (member? 'x '(a b x)) #t)
(test "member? - 1.revision" (member? 'b '(a (b) c)) #f)
(test "member? - 1.revision" (member? 'x '()) #f)

;; union: [set] [set] -> [set]
;; Page 27
(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2)
      (union (cdr set1) set2))
     (else (cons (car set1) (union (cdr set1) set2))))))

(test "union"
      (union '(stewed tomatoes and macaroni casserole) '(macaroni and cheese))
      '(stewed tomatoes casserole macaroni and cheese))

(test "union"
      (union '(a b c) '())
      '(a b c))

(test "union"
      (union '() '(x y z))
      '(x y z))

(test "union"
      (union '() '())
      '())

;; union: [set] [set] -> [set]
;; 1. revison of union
;; Page 28
(define union
  (lambda (set1 set2)
   (letrec
       ((U (lambda (set)
             (cond
              ((null? set) set2)
              ((member? (car set) set2)
               (U (cdr set)))
              (else (cons (car set) (U (cdr set))))))))
     (U set1))))

(test "union - 1.revision"
      (union '(stewed tomatoes and macaroni casserole) '(macaroni and cheese))
      '(stewed tomatoes casserole macaroni and cheese))

(test "union - 1.revision"
      (union '(a b c) '())
      '(a b c))

(test "union - 1.revision"
      (union '() '(x y z))
      '(x y z))

(test "union - 1.revision"
      (union '() '())
      '())

;; member?: [lat] [atom] -> boolean
;; 2. revision of member
;; Page 29
(define member?
  (lambda (lat a)
    (cond
     ((null? lat) #f)
     ((eq? (car lat) a) #t)
     (else (member? (cdr lat) a)))))

(test "member? - 2.revision" (member? '(a b c) 'x) #f)
(test "member? - 2.revision" (member? '(a x c) 'x) #t)
(test "member? - 2.revision" (member? '(x b c) 'x) #t)
(test "member? - 2.revision" (member? '(a b x) 'x) #t)
(test "member? - 2.revision" (member? '(a (b) c) 'b) #f)
(test "member? - 2.revision" (member? '() 'x) #f)

;; union: [set] [set] -> [set]
;; 2. revison of union
;; Page 31
(define union
  (lambda (set1 set2)
    (letrec
        ((U (lambda (set)
              (cond
               ((null? set) set2)
               ((M? (car set) set2)
                (U (cdr set)))
               (else (cons (car set) (U (cdr set)))))))
         (M? (lambda (a lat)
               (cond
                ((null? lat) #f)
                ((eq? (car lat) a) #t)
                (else (M? a (cdr lat)))))))
      (U set1))))

(test "union - 2.revision"
      (union '(stewed tomatoes and macaroni casserole) '(macaroni and cheese))
      '(stewed tomatoes casserole macaroni and cheese))

(test "union - 2.revision"
      (union '(a b c) '())
      '(a b c))

(test "union - 2.revision"
      (union '() '(x y z))
      '(x y z))

(test "union - 2.revision"
      (union '() '())
      '())

;; union: [set] [set] -> [set]
;; 3. revison of union
;; Page 32
(define union
  (lambda (set1 set2)
    (letrec
        ((U (lambda (set)
              (cond
               ((null? set) set2)
               ((M? (car set) set2)
                (U (cdr set)))
               (else (cons (car set) (U (cdr set)))))))
         (M? (lambda (a lat)
               (letrec
                   ((N? (lambda (l)
                          (cond
                           ((null? l) #f)
                           ((eq? (car l) a) #t)
                           (else (M? a (cdr l)))))))
                 (N? lat)))))
      (U set1))))

(test "union - 3.revision"
      (union '(stewed tomatoes and macaroni casserole) '(macaroni and cheese))
      '(stewed tomatoes casserole macaroni and cheese))

(test "union - 3.revision"
      (union '(a b c) '())
      '(a b c))

(test "union - 3.revision"
      (union '() '(x y z))
      '(x y z))

(test "union - 3.revision"
      (union '() '())
      '())

;; two-in-a-row?: [lat] -> boolean
;; Page 33
(define two-in-a-row?
  (lambda (lat)
    (letrec
        ((W (lambda (a lat)
              (cond
               ((null? lat) #f)
               (else (or (eq? (car lat) a)
                         (W (car lat) (cdr lat))))))))
      (cond
       ((null? lat) #f)
       (else (W (car lat) (cdr lat)))))))

(test "two-in-a-row?" (two-in-a-row? '(a b b c)) #t)
(test "two-in-a-row?" (two-in-a-row? '(a b c c)) #t)
(test "two-in-a-row?" (two-in-a-row? '(a a b c)) #t)
(test "two-in-a-row?" (two-in-a-row? '(a b c d)) #f)
(test "two-in-a-row?" (two-in-a-row? '()) #f)

;; two-in-a-row?: [lat] -> boolean
;; 1. revision of two-in-a-row?
;; Page 34
(define two-in-a-row?
  (letrec
      ((W (lambda (a lat)
            (cond
             ((null? lat) #f)
             (else (or (eq? (car lat) a)
                       (W (car lat) (cdr lat))))))))
    (lambda (lat)
      (cond
       ((null? lat) #f)
       (else (W (car lat) (cdr lat)))))))

(test "two-in-a-row? - 1.revision" (two-in-a-row? '(a b b c)) #t)
(test "two-in-a-row? - 1.revision" (two-in-a-row? '(a b c c)) #t)
(test "two-in-a-row? - 1.revision" (two-in-a-row? '(a a b c)) #t)
(test "two-in-a-row? - 1.revision" (two-in-a-row? '(a b c d)) #f)
(test "two-in-a-row? - 1.revision" (two-in-a-row? '()) #f)

;; sum-of-prefixes-b: [number] [tup] -> [number]
;; Page 10, Chapter 11
;; (define (sum-of-prefixes-b sonssf tup)
;;   (cond
;;    ((null? tup) '())
;;    (else (cons (+ sonssf (car tup))
;;            (sum-of-prefixes-b (+ sonssf (car tup)) (cdr tup))))))

;; sum-of-prefixes: [tup] -> [number]
;; Page 11, Chapter 11
;; (define (sum-of-prefixes tup)
;;   (sum-of-prefixes-b 0 tup))

;; sum-of-prefixes: [tup] -> [number]
;; Page 34
(define sum-of-prefixes
  (lambda (tup)
    (letrec
        ((S (lambda (sss tup)
              (cond
               ((null? tup) '())
               (else (cons (+ sss (car tup))
                       (S (+ sss (car tup)) (cdr tup))))))))
      (S 0 tup))))

(test "sum-of-prefixes" (sum-of-prefixes '(1 2 3 4)) '(1 3 6 10))
(test "sum-of-prefixes" (sum-of-prefixes '(3 5 7)) '(3 8 15))

;; scramble-b: [tup] [number] -> [tup]
;; Page 14, Chapter 11
;; (define (scramble-b tup rev-pre)
;;   (cond
;;    ((null? tup) '())
;;    (else
;;     (cons (pick (car tup) (cons (car tup) rev-pre))
;;       (scramble-b (cdr tup) (cons (car tup) rev-pre))))))

;; scramble: [tup] -> [tup]
;; Page 15, Chapter 11
;; (define (scramble tup)
;;   (scramble-b tup '()))

;; one?: number -> boolean
(define one?
  (lambda (n)
    (= n 1)))

(test "one?"
      (one? 3) #f)

(test "one?"
      (one? 0) #f)

(test "one?"
      (one? 1) #t)

;; sub1: number -> number
(define sub1
  (lambda (n)
    (- n 1)))

(test "sub1"
      (sub1 10) 9)

(test "sub1"
      (sub1 -4) -5)

;; pick: [number] [lat] -> [number]
;; Page 13, Chapter 11
(define pick
  (lambda (n lat)
    (cond
     ((one? n) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(test "pick" (pick 2 '(a b c d e)) 'b)
(test "pick" (pick 4 '(a b c d e)) 'd)
(test "pick" (pick 1 '(a b c d e)) 'a)
(test "pick" (pick 5 '(a b c d e)) 'e)

;; scramble: [tup] -> [tup]
;; Page 35
(define scramble
  (lambda (tup)
    (letrec
        ((P (lambda (tup rp)
              (cond
               ((null? tup) '())
               (else (cons (pick (car tup) (cons (car tup) rp))
                       (P (cdr tup) (cons (car tup) rp))))))))
      (P tup '()))))

(test "scramble"
      (scramble '(1 1 1 3 4 2 1 1 9 2))
      '(1 1 1 1 1 4 1 1 1 9))