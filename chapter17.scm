;; Chapter 17: We Change, Therefore We Are!

(load "test-check.scm")

;; atom?: any -> boolean
(define atom?
  (lambda (e)
    (and (not (pair? e)) (not (null? e)))))

;; add1: [number] -> [number]
(define add1
  (lambda (n)
    (+ n 1)))

;; sub1: [number] -> [number]
(define sub1
  (lambda (n)
    (- n 1)))

;; deep: [number] -> [s-exp]
;; Page 127
(define deep
  (lambda (m)
    (if (zero? m)
        'pizza
        (cons (deep (sub1 m)) '()))))

(test "deep" (deep 3) '(((pizza))))
(test "deep" (deep 7) '(((((((pizza))))))))
(test "deep" (deep 0) 'pizza)

;; find: [number] list list -> [s-exp] | boolean
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

(test "find" (find 4 '() '()) #f)
(test "find" (find 3 '(2 3 1) '(a b c)) 'b)

;; deepM: [number] -> [s-exp]
;; Page 127
(define deepM
  (let ((Rs '())
        (Ns '()))
    (letrec
        ((D (lambda (m)
              (if (zero? m)
                  'pizza
                  (cons (D (sub1 m)) '())))))
      (lambda (n)
        (let ((exists (find n Ns Rs)))
          (if (atom? exists)
              (let ((result (D n)))
                (set! Rs (cons result Rs))
                (set! Ns (cons n Ns))
                result)
              exists))))))

(test "deepM" (deepM 3) '(((pizza))))
(test "deepM" (deepM 6) '((((((pizza)))))))
(test "deepM" (deepM 3) '(((pizza))))

;; deepM: [number] -> [s-exp]
;; 1. revision of deepM
;; Page 127
(define deepM
  (let ((Rs '())
        (Ns '()))
    (letrec
        ((D (lambda (m)
              (if (zero? m)
                  'pizza
                  (cons (deepM (sub1 m)) '())))))
      (lambda (n)
        (let ((exists (find n Ns Rs)))
          (if (atom? exists)
              (let ((result (D n)))
                (set! Rs (cons result Rs))
                (set! Ns (cons n Ns))
                result)
              exists))))))

(test "deepM - 1.revision" (deepM 3) '(((pizza))))
(test "deepM - 1.revision" (deepM 6) '((((((pizza)))))))
(test "deepM - 1.revision" (deepM 3) '(((pizza))))

;; deepM: [number] -> [s-exp]
;; 2. revision of deepM
;; Page 127
(define deepM
  (let ((Rs '())
        (Ns '()))
    (let
        ((D (lambda (m)
              (if (zero? m)
                  'pizza
                  (cons (deepM (sub1 m)) '())))))
      (lambda (n)
        (let ((exists (find n Ns Rs)))
          (if (atom? exists)
              (let ((result (D n)))
                (set! Rs (cons result Rs))
                (set! Ns (cons n Ns))
                result)
              exists))))))

(test "deepM - 2.revision" (deepM 3) '(((pizza))))
(test "deepM - 2.revision" (deepM 6) '((((((pizza)))))))
(test "deepM - 2.revision" (deepM 3) '(((pizza))))

;; deepM: [number] -> [s-exp]
;; 3. revision of deepM
;; Page 128
(define deepM
  (let ((Rs '())
        (Ns '())
        (D (lambda (m)
             (if (zero? m)
                 'pizza
                 (cons (deepM (sub1 m)) '())))))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (D n)))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

(test "deepM - 3.revision" (deepM 3) '(((pizza))))
(test "deepM - 3.revision" (deepM 6) '((((((pizza)))))))
(test "deepM - 3.revision" (deepM 3) '(((pizza))))

;; deepM: [number] -> [s-exp]
;; 4. revision of deepM
;; Page 128
(define deepM
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result ((lambda (m)
                             (if (zero? m)
                                 'pizza
                                 (cons (deepM (sub1 m)) '())))
                           n)))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

(test "deepM - 4.revision" (deepM 3) '(((pizza))))
(test "deepM - 4.revision" (deepM 6) '((((((pizza)))))))
(test "deepM - 4.revision" (deepM 3) '(((pizza))))


;; deepM: [number] -> [s-exp]
;; 5. revision of deepM
;; Page 129
(define deepM
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (let ((m n))
                            (if (zero? m)
                                'pizza
                                (cons (deepM (sub1 m)) '())))))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

(test "deepM - 5.revision" (deepM 3) '(((pizza))))
(test "deepM - 5.revision" (deepM 6) '((((((pizza)))))))
(test "deepM - 5.revision" (deepM 3) '(((pizza))))

;; deepM: [number] -> [s-exp]
;; 6. revision of deepM
;; Page 129
(define deepM
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (if (zero? n)
                              'pizza
                              (cons (deepM (sub1 n)) '()))))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

(test "deepM - 6.revision" (deepM 3) '(((pizza))))
(test "deepM - 6.revision" (deepM 6) '((((((pizza)))))))
(test "deepM - 6.revision" (deepM 3) '(((pizza))))

;; consC: [s-exp] [s-exp] -> [s-exp]
;; Page 131
(define consC
  (let ((N 0))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))

(test "consC" (consC 'a '()) '(a))
(test "consC" (consC 'a '(b c)) '(a b c))

;; deep: [number] -> [s-exp]
;; Page 132
(define deep
  (lambda (m)
    (if (zero? m)
        'pizza
        (consC (deep (sub1 m)) '()))))

(test "deep" (deep 3) '(((pizza))))
(test "deep" (deep 7) '(((((((pizza))))))))
(test "deep" (deep 0) 'pizza)

;; You can write just (define counter) of course better it is better
;; to supply a value for variables.
(define counter #f)

;; consC: [s-exp] [s-exp] -> [s-exp]
;; Page 132
(define consC
  (let ((N 0))
    (set! counter (lambda () N))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))

(test "deep" (deep 5) '(((((pizza))))))
(test "counter" (counter) 5)

(test "deep" (deep 7) '(((((((pizza))))))))
(test "counter" (counter) 12)

(test "deep" (deep 0) 'pizza)
(test "counter" (counter) 12)

;; supercounter: fun -> [number]
;; Page 134
(define supercounter
  (lambda (f)
    (letrec
        ((S (lambda (n)
              (if (zero? n)
                  (f n)
                  (let ()
                    (f n)
                    (S (sub1 n)))))))
      (S 1000))))

;; supercounter: fun -> [number]
;; 1. revision
;; Page 134
(define supercounter
  (lambda (f)
    (letrec
        ((S (lambda (n)
              (if (zero? n)
                  (f n)
                  (let ()
                    (f n)
                    (S (sub1 n)))))))
      (S 1000)
      (counter))))

(test "supercounter" (supercounter deep) 500512)

(define counter #f)
(define set-counter #f)

;; consC: [s-exp] [s-exp] -> [s-exp]
;; Page 135
(define consC
  (let ((N 0))
    (set! counter (lambda () N))
    (set! set-counter (lambda (x) (set! N x)))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))

(set-counter 0)
(test "supercounter with set-counter" (supercounter deep) 500500)

;; deepM: [number] -> [s-exp]
;; 7. revision of deepM
;; Page 136
(define deepM
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (if (zero? n)
                              'pizza
                              (consC (deepM (sub1 n)) '()))))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

(test "deepM - 7.revision" (deepM 5) '(((((pizza))))))
(test "counter" (counter) 500505)

(set-counter 0)
(test "counter" (counter) 0)

;; deepM: [number] -> [s-exp]
;; 7. revision of deepM
;; Page 136
;;
;; We define deepM again to reset the local variables Rs and Ns to
;; '(). If you don't reset the local variables of deepM, evaluating
;; (deepM 5) then (supercounter deepM) results in the value of 995
;; instead of 1000.
(define deepM
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (if (zero? n)
                              'pizza
                              (consC (deepM (sub1 n)) '()))))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

(test "deepM - 7.revision" (deepM 5) '(((((pizza))))))
(test "counter" (counter) 5)
(test "deepM - 7.revision" (deepM 7) '(((((((pizza))))))))
(test "counter" (counter) 7)
(test "supercounter with set-counter" (supercounter deepM) 1000)

;; These procedures and variables are not part of The Seasoned
;; Schemer. You may want to skip them. This revised deepM procedure
;; allows you to inspect its local variables Ns and Rs.
;; 
;; (define set-Ns #f)
;; (define set-Rs #f)
;; (define show-Ns #f)
;; (define show-Rs #f)
;;
;; deepM: [number] -> [s-exp]
;; 8. revision of deepM
;;
;; This version of deepM is like the 7.revision but you can also
;; inspect the state of the local variables Ns and Rs with the
;; procedures 'show-Rs' and 'show-Ns'. Moreover you can also change
;; these with 'set-Ns' and 'set-Rs'.
;; (define deepM
;;   (let ((Rs '())
;;         (Ns '()))
;;     (set! show-Ns (lambda () Ns))
;;     (set! set-Ns (lambda (n) (set! Ns n)))
;;     (set! set-Rs (lambda (x) (set! Rs x)))
;;     (set! show-Rs (lambda () Rs))
;;     (lambda (n)
;;       (let ((exists (find n Ns Rs)))
;;         (if (atom? exists)
;;             (let ((result (if (zero? n)
;;                               'pizza
;;                               (consC (deepM (sub1 n)) '()))))
;;               (set! Rs (cons result Rs))
;;               (set! Ns (cons n Ns))
;;               result)
;;             exists)))))

;; rember1*: [atom] list -> list
;; Page 139
(define rember1*
  (lambda (a l)
    (letrec
        ((R (lambda (l oh)
              (cond
               ((null? l) (oh 'no))
               ((atom? (car l)) (if (eq? (car l) a)
                                    (cdr l)
                                    (cons (car l)
                                      (R (cdr l) oh))))
               (else
                (let ((new-car (call/cc
                                (lambda (oh)
                                  (R (car l) oh)))))
                  (if (atom? new-car)
                      (cons (car l)
                        (R (cdr l) oh))
                      (cons new-car (cdr l)))))))))
      (let ((new-l (call/cc
                    (lambda (oh)
                      (R l oh)))))
        (if (atom? new-l)
            l
            new-l)))))

(test "rember1*" (rember1* 'food '((food) more (food))) '(() more (food)))
(test "rember1*" (rember1* 'x '(a x b c x d)) '(a b c x d))

;; rember1*C: [atom] list -> list
;; Page 139
(define rember1*C
  (lambda (a l)
    (letrec
        ((R (lambda (l oh)
              (cond
               ((null? l) (oh 'no))
               ((atom? (car l)) (if (eq? (car l) a)
                                    (cdr l)
                                    (consC (car l)
                                           (R (cdr l) oh))))
               (else
                (let ((new-car (call/cc
                                (lambda (oh)
                                  (R (car l) oh)))))
                  (if (atom? new-car)
                      (consC (car l)
                             (R (cdr l) oh))
                      (consC new-car (cdr l)))))))))
      (let ((new-l (call/cc
                    (lambda (oh)
                      (R l oh)))))
        (if (atom? new-l)
            l
            new-l)))))

(set-counter 0)
(test "rember1*C" (rember1*C 'noodles '((food) more (food))) '((food) more (food)))
(test "counter" (counter) 0)


;; eqlist?: [listof s-exp] [listof s-exp] -> boolean
;; 2. revision of eqlist?
;;
;; Page 93, The Little Schemer
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

;; rember1*: [atom] list -> list
;; 1.revision
;; Page 140
(define rember1*
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
               ((null? l) '())
               ((atom? (car l)) (if (eq? (car l) a)
                                    (cdr l)
                                    (cons (car l)
                                      (R (cdr l)))))
               (else
                (let ((av (R (car l))))
                  (if (eqlist? (car l) av)
                      (cons (car l)
                        (R (cdr l)))
                      (cons av (cdr l)))))))))
      (R l))))

(test "rember1* - 1.revision" (rember1* 'food '((food) more (food))) '(() more (food)))
(test "rember1* - 1.revision" (rember1* 'x '(a x b c x d)) '(a b c x d))

;; rember1*C2: [atom] list -> list
;; Page 140
(define rember1*C2
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
               ((null? l) '())
               ((atom? (car l)) (if (eq? (car l) a)
                                    (cdr l)
                                    (consC (car l)
                                           (R (cdr l)))))
               (else
                (let ((av (R (car l))))
                  (if (eqlist? (car l) av)
                      (consC (car l)
                             (R (cdr l)))
                      (consC av (cdr l)))))))))
      (R l))))

(set-counter 0)
(test "consC"
      (consC (consC 'food '())
             (consC 'more
                    (consC (consC 'food '()) '())))
      '((food) more (food)))
(test "counter" (counter) 5)

(set-counter 0)
(test "rember1*C2" (rember1*C2 'noodles '((food) more (food))) '((food) more (food)))
(test "counter" (counter) 5)


