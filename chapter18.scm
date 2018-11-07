;; Chapter 18: We Change, Therefore We Are the Same!

(load "test-check.scm")

;; add1: [number] -> [number]
(define add1
  (lambda (n)
    (+ n 1)))

;; sub1: [number] -> [number]
(define sub1
  (lambda (n)
    (- n 1)))

;; kons: [s-exp] [s-exp] -> [pair]
(define kons cons)

;; lots: [number] -> list
;; Page 143
(define lots
  (lambda (m)
    (cond
     ((zero? m) '())
     (else (kons 'egg (lots (sub1 m)))))))

(test "lots" (lots 3) '(egg egg egg))
(test "lots" (lots 5) '(egg egg egg egg egg))
(test "lots" (lots 10) '(egg egg egg egg egg egg egg egg egg egg))

;; kar: [pair] -> [s-exp]
(define kar car)

;; kar: list -> list
(define kdr cdr)

;; lenkth: list -> [number]
;; Page 143
(define lenkth
  (lambda (l)
    (cond
     ((null? l) 0)
     (else (add1 (lenkth (kdr l)))))))

(test "lenkth" (lenkth '(egg egg egg)) 3)
(test "lenkth" (lenkth '()) 0)


(define kounter #f)
(define set-kounter #f)

;; konsC: [s-exp] [s-exp] -> [s-exp]
(define konsC
  (let ((N 0))
    (set! kounter (lambda () N))
    (set! set-kounter (lambda (x) (set! N x)))
    (lambda (x y)
      (set! N (add1 N))
      (kons x y))))

(test "konsC" (konsC 'a '(b)) '(a b))
(test "kounter" (kounter) 1)

(set-kounter 0)

;; add-at-end: list -> list
;; Page 144
(define add-at-end
  (lambda (l)
    (cond
     ((null? (kdr l)) (konsC (kar l) (kons 'egg '())))
     (else (konsC (kar l) (add-at-end (kdr l)))))))

(test "add-at-end" (add-at-end (lots 3)) '(egg egg egg egg))
(test "kounter" (kounter) 3)

;; set-kdr: list -> void
(define set-kdr set-cdr!)

;; add-at-end-too: list -> list
;; Page 145
(define add-at-end-too
  (lambda (l)
    (letrec
        ((A (lambda (ls)
              (cond
               ((null? (kdr ls)) (set-kdr ls (kons 'egg '())))
               (else (A (kdr ls)))))))
      (A l)
      l)))

(set-kounter 0)
(test "add-at-end-too" (add-at-end-too (lots 3)) '(egg egg egg egg))
(test "kounter" (kounter) 0)

;; kons: [s-exp] [s-exp] -> fun
;; Page 146
(define kons
  (lambda (kar cdr)
    (lambda (selector)
      (selector kar cdr))))

;; kar: fun -> [s-exp]
;; Page 146
(define kar
  (lambda (c)
    (c (lambda (a d) a))))

;; kdr: fun -> [list]
;; Page 146
(define kdr
  (lambda (c)
    (c (lambda (a d) d))))

(test "kons and kar" (kar (kons 'a '())) 'a)
(test "kons and kar" (kar (kons 'a '(b))) 'a)
(test "kons and kdr" (kdr (kons 'a '())) '())
(test "kons and kdr" (kdr (kons 'a '(b))) '(b))

;; bons: [s-exp] -> fun
;; Page 146
(define bons
  (lambda (kar)
    (let ((kdr '()))
      (lambda (selector)
        (selector (lambda (x) (set! kdr x))
                  kar
                  kdr)))))

;; kar: fun -> [s-exp]
;; 1. revision
;; Page 146
(define kar
  (lambda (c)
    (c (lambda (s a d) a))))

;; kdr: fun -> [s-exp]
;; 1. revision
;; Page 146
(define kdr
  (lambda (c)
    (c (lambda (s a d) d))))

(test "bons and kar - 1.revision" (kar (bons 'egg)) 'egg)
(test "bons and kar - 1.revision" (kdr (bons 'egg)) '())

;; set-kdr: fun [s-exp] -> void
;; Page 147
(define set-kdr
  (lambda (c x)
    ((c (lambda (s a d) s)) x)))

;; kons: [s-exp] [s-exp] -> fun
;; 1.revision
;; Page 146
(define kons
  (lambda (a d)
    (let ((c (bons a)))
      (set-kdr c d)
      c)))

(test "kons and kar - 1.revision" (kar (kons 'a '())) 'a)
(test "kons and kar - 1.revision" (kar (kons 'a '(b))) 'a)
(test "kons and kdr - 1.revision" (kdr (kons 'a '())) '())
(test "kons and kdr - 1.revision" (kdr (kons 'a '(b))) '(b))

;; Page 147
(define dozen (lots 12))

(test "dozen, kar" (kar dozen) 'egg)
(test "dozen, kar, kdr" (kar (kdr dozen)) 'egg)
(test "dozen, lenkth" (lenkth dozen) 12)

;; Page 147
(define bakers-dozen (add-at-end dozen))

(test "bakers-dozen" (lenkth bakers-dozen) 13)

;; Page 148
(define bakers-dozen-too (add-at-end-too dozen))
(test "bakers-dozen-too" (lenkth bakers-dozen-too) 13)

;; Page 148
(define bakers-dozen-again (add-at-end dozen))
(test "bakers-dozen-again" (lenkth bakers-dozen-again) 14)

;; eklist?: list list -> boolean
;; Page 149
(define eklist?
  (lambda (ls1 ls2)
    (cond
     ((null? ls1) (null? ls2))
     ((null? ls2) #f)
     (else
      (and (eq? (kar ls1) (kar ls2))
           (eklist? (kdr ls1) (kdr ls2)))))))

(test "eklist?" (eklist? bakers-dozen bakers-dozen-too) #t)

;; same?: [s-exp] [s-exp] -> boolean
;; Page 150
(define same?
  (lambda (c1 c2)
    (let ((t1 (kdr c1))
          (t2 (kdr c2)))
      (set-kdr c1 1)
      (set-kdr c2 2)
      (let ((v (= (kdr c1) (kdr c2))))
        (set-kdr c1 t1)
        (set-kdr c2 t2)
        v))))

;; Test is #f because (add-at-end dozen) creates new kons pairs, thus
;; a kons of bakers-dozen is not the 'same' as a kons of
;; bakers-dozen-too.
(test "same?" (same? bakers-dozen bakers-dozen-too) #f)

;; Test is #t because add-at-end-too does not create new
;; konses. add-at-end-too just walk along the kdrs until the end of
;; the kdr chain is reached. There add-at-end-too appends a kons
;; pair destructivly. Thus changing also 'dozen' too.
(test "same?" (same? dozen bakers-dozen-too) #t)

;; Test is #f because even so the value of the kons pairs is equal
;; they are not the same kons pairs.
(test "same?" (same? (kons 'egg '()) (kons 'egg '())) #f)


;; last-kons: list -> list
;; Page 151
(define last-kons
  (lambda (ls)
    (cond
     ((null? (kdr ls)) ls)
     (else (last-kons (kdr ls))))))

(define long (lots 12))

(test "last-kons" (kar (last-kons long)) 'egg)

;; o+: number number -> number
;; Returns the addition of 'n' and 'm'
;;
;; Page 60, The Little Schemer
(define o+
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (o+ n (sub1 m)))))))

(test "o+"
      (o+ 2 7) 9)

(test "o+"
      (o+ 2 0) 2)

;; finite-lenkth: list -> boolean
;; Page 153
(define finite-lenkth
  (lambda (p)
    (call/cc
     (lambda (infinite)
       (letrec
           ((C (lambda (p q)
                 (cond
                  ((same? p q) (infinite #f))
                  ((null? (kdr q)) 1)
                  (else
                   (o+ (C (sl p) (qk q)) 2)))))
            (qk (lambda (x) (kdr (kdr x))))
            (sl (lambda (x) (kdr x))))
         (cond
          ((null? p) 0)
          (else
           (add1 (C p (kdr p))))))))))

;; Create circular list. The kdr of 'long' points to 'long' itself. 
(set-kdr (last-kons long) long)

;; finite-lenkth returns #f if its arguments is an circular list. Test
;; also (lenkth long) and see what happens.
(test "circular list" (finite-lenkth long) #f)
