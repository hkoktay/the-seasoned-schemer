;; Chapter 14: Let There Be Names

(load "test-check.scm")

;; atom?: any -> boolean
(define atom?
  (lambda (e)
    (and (not (null? e)) (not (pair? e)))))

;; eqlist?: [s-exp] [s-exp] -> boolean
;; Page 93, Chapter 5, The Little Schemer
(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else
      (and (equal? (car l1) (car l2))
           (eqlist? (cdr l1) (cdr l2)))))))

(test "eqlist?"
      (eqlist? '(strawberry ice cream) '(strawberry ice cream))
      #t)

(test "eqlist?"
      (eqlist? '(strawberry ice cream) '(strawberry cream ice))
      #f)

(test "eqlist?"
      (eqlist? '(banana ((split))) '((banana) (split)))
      #f)

(test "eqlist?"
      (eqlist? '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda))))
      #f)

(test "eqlist?"
      (eqlist? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda))))
      #t)

;; leftmost: [s-exp] -> [s-exp]
;; Page 63
(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))

(test "leftmost"
      (leftmost '((potato) (chips ((with) fish) (chips))))
      'potato)

(test "leftmost"
      (leftmost '(((hot) (tuna (and))) cheese))
      'hot)

;; leftmost: [s-exp] -> [atom]
;; 1. revision of leftmost
;; 65
(define leftmost
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (car l))
     (else (cond
            ((atom? (leftmost (car l))) (leftmost (car l)))
            (else (leftmost (cdr l))))))))

(test "leftmost - 1.revision"
      (leftmost '((potato) (chips ((with) fish) (chips))))
      'potato)

(test "leftmost - 1.revision"
      (leftmost '(((hot) (tuna (and))) cheese))
      'hot)

;; leftmost: [s-exp] -> [atom]
;; 2. revision of leftmost
;; 66
(define leftmost
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (car l))
     (else (let ((a (leftmost (car l))))
             (cond
              ((atom? a) a)
              (else (leftmost (cdr l)))))))))

(test "leftmost - 2.revision"
      (leftmost '((potato) (chips ((with) fish) (chips))))
      'potato)

(test "leftmost - 2.revision"
      (leftmost '(((hot) (tuna (and))) cheese))
      'hot)

;; rember1*: [atom] [s-exp] -> [s-exp]
;; Page 67
(define rember1*
  (lambda (a l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (cond
                       ((eq? (car l) a) (cdr l))
                       (else (cons (car l)
                               (rember1* a (cdr l))))))
     (else (cond
            ((eqlist? (rember1* a (car l)) (car l))
             (cons (car l) (rember1* a (cdr l))))
            (else (cons (rember1* a (car l))
                    (cdr l))))))))

(test "rember1*"
      (rember1* 'salad '((Swedish rye) (French (mustard salad turkey)) salad))
      '((Swedish rye) (French (mustard turkey)) salad))

(test "rember1*"
      (rember1* 'meat '((pasta meat) pasta (noodles meat sauce) meat tomatoes))
      '((pasta) pasta (noodles meat sauce) meat tomatoes))

;; rember1*: [atom] [s-exp] -> [s-exp]
;; Page 67
;; 1. revision of rember1*
(define rember1*
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
               ((null? l) '())
               ((atom? (car l)) (cond
                                 ((eq? (car l) a) (cdr l))
                                 (else (cons (car l)
                                         (R (cdr l))))))
               (else (cond
                      ((eqlist? (R (car l)) (car l))
                       (cons (car l) (R (cdr l))))
                      (else (cons (R (car l)) (cdr l)))))))))
      (R l))))

(test "rember1* - 1.revision"
      (rember1* 'salad '((Swedish rye) (French (mustard salad turkey)) salad))
      '((Swedish rye) (French (mustard turkey)) salad))

(test "rember1* - 1.revision"
      (rember1* 'meat '((pasta meat) pasta (noodles meat sauce) meat tomatoes))
      '((pasta) pasta (noodles meat sauce) meat tomatoes))

;; rember1*: [atom] [s-exp] -> [s-exp]
;; Page 68
;; 2. revision of rember1*
(define rember1*
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
               ((null? l) '())
               ((atom? (car l)) (cond
                                 ((eq? (car l) a) (cdr l))
                                 (else (cons (car l)
                                         (R (cdr l))))))
               (else (let ((av (R (car l))))
                       (cond
                        ((eqlist? av (car l))
                         (cons (car l) (R (cdr l))))
                        (else (cons av (cdr l))))))))))
      (R l))))

(test "rember1* - 2.revision"
      (rember1* 'salad '((Swedish rye) (French (mustard salad turkey)) salad))
      '((Swedish rye) (French (mustard turkey)) salad))

(test "rember1* - 2.revision"
      (rember1* 'meat '((pasta meat) pasta (noodles meat sauce) meat tomatoes))
      '((pasta) pasta (noodles meat sauce) meat tomatoes))

;; add1: number -> number
(define add1
  (lambda (n)
    (+ n 1)))

(test "add1" (add1 67) 68)
(test "add1" (add1 0) 1)
(test "add1" (add1 -4) -3)

;; depth*: [s-exp] -> [number]
;; Page 69
(define depth*
  (lambda (l)
    (cond
     ((null? l) 1)
     ((atom? (car l)) (depth* (cdr l)))
     (else (cond
            ((> (depth* (cdr l)) (add1 (depth* (car l))))
             (depth* (cdr l)))
            (else (add1 (depth* (car l)))))))))

(test "depth*"
      (depth* '((pickled) peppers (peppers pickled)))
      2)

(test "depth*"
      (depth* '(margarine ((bitter butter) (makes) (batter (bitter))) butter))
      4)

(test "depth*"
      (depth* '(c (b (a b) a) a))
      3)

;; depth*: [s-exp] -> [number]
;; 1. revision of depth*
;; This version does not work. The conditional (null? l) must be
;; tested before a (car l) is evaluated.
;; Page 70
(define depth*
  (lambda (l)
    (let ((a (add1 (depth* (car l))))
          (d (depth* (cdr l))))
      (cond
       ((null? l) 1)
       ((atom? (car l)) d)
       (else (cond
              ((> d a) d)
              (else a)))))))

;; depth*: [s-exp] -> [number]
;; 2. revision of depth*
;; This version corrects the 1. revision of depth*
;; Page 72
(define depth*
  (lambda (l)
    (cond
     ((null? l) 1)
     ((atom? (car l)) (depth* (cdr l)))
     (else (let ((a (add1 (depth* (car l))))
                 (d (depth* (cdr l))))
             (cond
              ((> d a) d)
              (else a)))))))

(test "depth* - 2.revision"
      (depth* '((pickled) peppers (peppers pickled)))
      2)

(test "depth* - 2.revision"
      (depth* '(margarine ((bitter butter) (makes) (batter (bitter))) butter))
      4)

(test "depth* - 2.revision"
      (depth* '(c (b (a b) a) a))
      3)

(test "depth* - 2.revision"
      (depth* '(() ((bitter butter) (makes) (batter (bitter))) butter))
      4)

;; depth*: [s-exp] -> [number]
;; 3. revision of depth*
;; This version corrects the 1. revision of depth*
;; Page 73
(define depth*
  (lambda (l)
    (cond
     ((null? l) 1)
     (else (let ((d (depth* (cdr l))))
             (cond
              ((atom? (car l)) d)
              (else (cond
                     ((> d (add1 (depth* (car l)))) d)
                     (else (add1 (depth* (car l))))))))))))

(test "depth* - 3.revision"
      (depth* '((pickled) peppers (peppers pickled)))
      2)

(test "depth* - 3.revision"
      (depth* '(margarine ((bitter butter) (makes) (batter (bitter))) butter))
      4)

(test "depth* - 3.revision"
      (depth* '(c (b (a b) a) a))
      3)

(test "depth* - 3.revision"
      (depth* '(() ((bitter butter) (makes) (batter (bitter))) butter))
      4)

;; depth*: [s-exp] -> [number]
;; 4. revision of depth*
;; This version corrects the 1. revision of depth*
;; Page 73
(define depth*
  (lambda (l)
    (cond
     ((null? l) 1)
     (else (let ((d (depth* (cdr l))))
             (cond
              ((atom? (car l)) d)
              (else (let ((a (add1 (depth* (car l)))))
                      (cond
                       ((> d a) d)
                       (else a))))))))))

(test "depth* - 4.revision"
      (depth* '((pickled) peppers (peppers pickled)))
      2)

(test "depth* - 4.revision"
      (depth* '(margarine ((bitter butter) (makes) (batter (bitter))) butter))
      4)

(test "depth* - 4.revision"
      (depth* '(c (b (a b) a) a))
      3)

(test "depth* - 4.revision"
      (depth* '(() ((bitter butter) (makes) (batter (bitter))) butter))
      4)

;; depth*: [s-exp] -> [number]
;; 5. revision of depth*
;; Page 75
(define depth*
  (lambda (l)
    (cond
     ((null? l) 1)
     (else (let ((d (depth* (cdr l))))
             (cond
              ((atom? (car l)) d)
              (else (let ((a (add1 (depth* (car l))))
                          (d (depth* (cdr l))))
                      (if (> d a) d a)))))))))

(test "depth* - 5.revision"
      (depth* '((pickled) peppers (peppers pickled)))
      2)

(test "depth* - 5.revision"
      (depth* '(margarine ((bitter butter) (makes) (batter (bitter))) butter))
      4)

(test "depth* - 5.revision"
      (depth* '(c (b (a b) a) a))
      3)

(test "depth* - 5.revision"
      (depth* '(() ((bitter butter) (makes) (batter (bitter))) butter))
      4)

;; depth*: [s-exp] -> [number]
;; 6. revision of depth*
;; Page 75
(define depth*
  (lambda (l)
    (cond
     ((null? l) 1)
     (else (let ((d (depth* (cdr l))))
             (cond
              ((atom? (car l)) d)
              (else (let ((a (add1 (depth* (car l))))
                          (d (depth* (cdr l))))
                      (max a d)))))))))

(test "depth* - 6.revision"
      (depth* '((pickled) peppers (peppers pickled)))
      2)

(test "depth* - 6.revision"
      (depth* '(margarine ((bitter butter) (makes) (batter (bitter))) butter))
      4)

(test "depth* - 6.revision"
      (depth* '(c (b (a b) a) a))
      3)

(test "depth* - 6.revision"
      (depth* '(() ((bitter butter) (makes) (batter (bitter))) butter))
      4)

;; depth*: [s-exp] -> [number]
;; 7. revision of depth*
;; Page 75
(define depth*
  (lambda (l)
    (cond
     ((null? l) 1)
     (else (let ((d (depth* (cdr l))))
             (cond
              ((atom? (car l)) d)
              (else (max
                     (add1 (depth* (car l)))
                     (depth* (cdr l))))))))))

(test "depth* - 7.revision"
      (depth* '((pickled) peppers (peppers pickled)))
      2)

(test "depth* - 7.revision"
      (depth* '(margarine ((bitter butter) (makes) (batter (bitter))) butter))
      4)

(test "depth* - 7.revision"
      (depth* '(c (b (a b) a) a))
      3)

(test "depth* - 7.revision"
      (depth* '(() ((bitter butter) (makes) (batter (bitter))) butter))
      4)

;; max: [number] [number] -> [number]
(define max
  (lambda (n m)
    (if (> n m) n m)))

(test "max" (max 2 4) 4)
(test "max" (max 7 4) 7)
(test "max" (max 1 1) 1)

;; depth*: [s-exp] -> [number]
;; 6. revision of depth*
;; Page 75
(define depth*
  (lambda (l)
    (cond
     ((null? l) 1)
     ((atom? (car l)) (depth* (cdr l)))
     (else (let ((a (add1 (depth* (car l))))
                 (d (depth* (cdr l))))
             (max a d))))))

(test "depth* - 6.revision"
      (depth* '((pickled) peppers (peppers pickled)))
      2)

(test "depth* - 6.revision"
      (depth* '(margarine ((bitter butter) (makes) (batter (bitter))) butter))
      4)

(test "depth* - 6.revision"
      (depth* '(c (b (a b) a) a))
      3)

(test "depth* - 6.revision"
      (depth* '(() ((bitter butter) (makes) (batter (bitter))) butter))
      4)

;; depth*: [s-exp] -> [number]
;; 7. revision of depth*
;; Page 75
(define depth*
  (lambda (l)
    (cond
     ((null? l) 1)
     ((atom? (car l)) (depth* (cdr l)))
     (else (max (add1 (depth* (car l)))
                (depth* (cdr l)))))))

(test "depth* - 7.revision"
      (depth* '((pickled) peppers (peppers pickled)))
      2)

(test "depth* - 7.revision"
      (depth* '(margarine ((bitter butter) (makes) (batter (bitter))) butter))
      4)

(test "depth* - 7.revision"
      (depth* '(c (b (a b) a) a))
      3)

(test "depth* - 7.revision"
      (depth* '(() ((bitter butter) (makes) (batter (bitter))) butter))
      4)

;; scramble: [tup] -> [tup]
;; Page 35, Chapter 12
;; (define scramble
;;   (lambda (tup)
;;     (letrec
;;         ((P (lambda (tup rp)
;;               (cond
;;                ((null? tup) '())
;;                (else (cons (pick (car tup) (cons (car tup) rp))
;;                        (P (cdr tup) (cons (car tup) rp))))))))
;;       (P tup '()))))

;; one?: [number] -> boolean
;; Returns #t if 'n' is 1, else #f
;; Page 79, Chapter 4
(define one?
  (lambda (n)
    (= n 1)))

(test "one?" (one? 3) #f)
(test "one?" (one? 0) #f)
(test "one?" (one? 1) #t)

;; sub1: number -> number
(define sub1
  (lambda (n)
    (- n 1)))

(test "sub1" (sub1 10) 9)

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
;; Page 76
(define scramble
  (lambda (tup)
    (letrec
        ((P (lambda (tup rp)
              (cond
               ((null? tup) '())
               (else (let ((rp (cons (car tup) rp)))
                       (cons (pick (car tup) rp)
                         (P (cdr tup) rp))))))))
      (P tup '()))))

(test "scramble"
      (scramble '(1 1 1 3 4 2 1 1 9 2))
      '(1 1 1 1 1 4 1 1 1 9))

;; lm: [s-exp] [cont] -> [s-exp]
(define lm
  (lambda (l out)
    (cond
     ((null? l) '())
     ((atom? (car l)) (out (car l)))
     (else (let ()
             (lm (car l) out)
             (lm (cdr l) out))))))

;; leftmost: [s-exp] -> [atom]
;; 3. revision of leftmost
;; Page 78
(define leftmost
  (lambda (l)
    (call/cc
     (lambda (skip)
       (lm l skip)))))

(test "leftmost - 3.revision"
      (leftmost '((potato) (chips ((with) fish) (chips))))
      'potato)

(test "leftmost - 3.revision"
      (leftmost '(((hot) (tuna (and))) cheese))
      'hot)

(test "leftmost - 3.revision"
      (leftmost '(((a)) b (c)))
      'a)

;; leftmost: [s-exp] -> [atom]
;; 4. revision of leftmost
;; Page 81
(define leftmost
  (letrec
      ((lm (lambda (l out)
             (cond
              ((null? l) '())
              ((atom? (car l)) (out (car l)))
              (else (let ()
                      (lm (car l) out)
                      (lm (cdr l) out)))))))
    (lambda (l)
      (call/cc
       (lambda (skip)
         (lm l skip))))))

(test "leftmost - 4.revision"
      (leftmost '((potato) (chips ((with) fish) (chips))))
      'potato)

(test "leftmost - 4.revision"
      (leftmost '(((hot) (tuna (and))) cheese))
      'hot)

(test "leftmost - 4.revision"
      (leftmost '(((a)) b (c)))
      'a)

;; leftmost: [s-exp] -> [atom]
;; 5. revision of leftmost
;; Page 81
(define leftmost
  (lambda (l)
    (letrec
        ((lm (lambda (l out)
               (cond
                ((null? l) '())
                ((atom? (car l)) (out (car l)))
                (else (let ()
                        (lm (car l) out)
                        (lm (cdr l) out)))))))
      (call/cc
       (lambda (skip)
         (lm l skip))))))

(test "leftmost - 5.revision"
      (leftmost '((potato) (chips ((with) fish) (chips))))
      'potato)

(test "leftmost - 5.revision"
      (leftmost '(((hot) (tuna (and))) cheese))
      'hot)

(test "leftmost - 5.revision"
      (leftmost '(((a)) b (c)))
      'a)

;; leftmost: [s-exp] -> [atom]
;; 6. revision of leftmost
;; Page 81
(define leftmost
  (lambda (l)
    (call/cc
     (lambda (skip)
       (letrec
           ((lm (lambda (l out)
                  (cond
                   ((null? l) '())
                   ((atom? (car l)) (out (car l)))
                   (else (let ()
                           (lm (car l) out)
                           (lm (cdr l) out)))))))
         (lm l skip))))))

(test "leftmost - 6.revision"
      (leftmost '((potato) (chips ((with) fish) (chips))))
      'potato)

(test "leftmost - 6.revision"
      (leftmost '(((hot) (tuna (and))) cheese))
      'hot)

(test "leftmost - 6.revision"
      (leftmost '(((a)) b (c)))
      'a)

;; leftmost: [s-exp] -> [atom]
;; 7. revision of leftmost
;; Page 82
(define leftmost
  (lambda (l)
    (call/cc
     (lambda (skip)
       (letrec
           ((lm (lambda (l skip)
                  (cond
                   ((null? l) '())
                   ((atom? (car l)) (skip (car l)))
                   (else (let ()
                           (lm (car l) skip)
                           (lm (cdr l) skip)))))))
         (lm l skip))))))

(test "leftmost - 7.revision"
      (leftmost '((potato) (chips ((with) fish) (chips))))
      'potato)

(test "leftmost - 7.revision"
      (leftmost '(((hot) (tuna (and))) cheese))
      'hot)

(test "leftmost - 7.revision"
      (leftmost '(((a)) b (c)))
      'a)

;; leftmost: [s-exp] -> [atom]
;; 8. revision of leftmost
;; Page 82
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

(test "leftmost - 8.revision"
      (leftmost '((potato) (chips ((with) fish) (chips))))
      'potato)

(test "leftmost - 8.revision"
      (leftmost '(((hot) (tuna (and))) cheese))
      'hot)

(test "leftmost - 8.revision"
      (leftmost '(((a)) b (c)))
      'a)

;; rem: [atom] [s-exp] [cont] -> [s-exp]
;; Page 85
(define rm
  (lambda (a l oh)
    (cond
     ((null? l) (oh 'no))
     ((atom? (car l)) (if (eq? (car l) a)
                          (cdr l)
                          (cons (car l)
                            (rm a (cdr l) oh))))
     (else (if (atom? (call/cc
                       (lambda (oh)
                         (rm a (car l) oh))))
               (cons (car l)
                 (rm a (cdr l) oh))
               (cons (rm a (car l) oh)
                 (cdr l)))))))

(test "rm"
      (call/cc (lambda (Say) (rm 'noodles '((food) more (food)) Say)))
      'no)

;; rember1*: [atom] [lat] -> [lat]
;; 3. revison of rember1*
;; Page 87
(define rember1*
  (lambda (a l)
    (if (atom? (call/cc
                (lambda (oh)
                  (rm a l oh))))
        l
        (rm a l '()))))

(test "rember1* - 3.revision"
      (rember1* 'salad '((Swedish rye) (French (mustard salad turkey)) salad))
      '((Swedish rye) (French (mustard turkey)) salad))

(test "rember1* - 3.revision"
      (rember1* 'meat '((pasta meat) pasta (noodles meat sauce) meat tomatoes))
      '((pasta) pasta (noodles meat sauce) meat tomatoes))

;; rember1*: [atom] [lat] -> [lat]
;; 4. revison of rember1*
;; Page 88
(define rember1*
  (lambda (a l)
    (let ((new-l (call/cc
                  (lambda (oh)
                    (rm a l oh)))))
      (if (atom? new-l)
          l
          new-l))))

(test "rember1* - 4.revision"
      (rember1* 'salad '((Swedish rye) (French (mustard salad turkey)) salad))
      '((Swedish rye) (French (mustard turkey)) salad))

(test "rember1* - 4.revision"
      (rember1* 'meat '((pasta meat) pasta (noodles meat sauce) meat tomatoes))
      '((pasta) pasta (noodles meat sauce) meat tomatoes))

;; rm: [atom] [lat] [cont] -> [lat]
;; 1. revison of rm
;; Page 88
(define rm
  (lambda (a l oh)
    (cond
     ((null? l) (oh 'no))
     ((atom? (car l)) (if (eq? (car l) a)
                          (cdr l)
                          (cons (car l)
                            (rm a (cdr l) oh))))
     (else (let ((new-car (call/cc
                           (lambda (oh)
                             (rm a (car l) oh)))))
             (if (atom? new-car)
                 (cons (car l)
                   (rm a (cdr l) oh))
                 (cons new-car (cdr l))))))))

(test "rm - 1.revision"
      (call/cc (lambda (Say) (rm 'noodles '((food) more (food)) Say)))
      'no)

(define try
  (lambda (x alpha beta)
    (call/cc
     (lambda (success)
       (call/cc
        (lambda (x)
          (success alpha)))
       beta))))

;; rember1*: [atom] [lat] -> [lat]
;; 4. revison of rember1*
;; Page 89
(define rember1*
  (lambda (a l)
    (try oh (rm a l oh) l)))

;; rm: [atom] [lat] [cont] -> [lat]
;; 2. revison of rm
;; Page 89
(define rm
  (lambda (a l oh)
    (cond
     ((null? l) (oh 'no))
     ((atom? (car l)) (if (eq? (car l) a)
                          (cdr l)
                          (cons (car l)
                            (rm a (cdr l) oh))))
     (else (try oh
                (cons (rm a (car l) oh)
                  (cdr l))
                (cons (car l)
                  (rm a (cdr l) oh)))))))

(test "rm - 2.revision"
      (call/cc (lambda (Say) (rm 'noodles '((food) more (food)) Say)))
      'no)
