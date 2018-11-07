;; Chapter 13

(load "test-check.scm")

;; member?: [atom] [lat] -> boolean
(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     ((eq? a (car lat)) #t)
     (else (member? a (cdr lat))))))

(test "member?" (member? 'x '(a b c)) #f)
(test "member?" (member? 'x '(a x c)) #t)
(test "member?" (member? 'x '(x b c)) #t)
(test "member?" (member? 'x '(a b x)) #t)
(test "member?" (member? 'b '(a (b) c)) #f)
(test "member?" (member? 'x '()) #f)

;; intersect: [set] [set] -> [set]
;; Page 37
(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2)
      (cons (car set1) (intersect (cdr set1) set2)))
     (else (intersect (cdr set1) set2)))))

(test "intersect"
      (intersect '(a b c) '(x b y))
      '(b))

(test "intersect"
      (intersect '() '(x b y))
      '())

(test "intersect"
      (intersect '(a b c) '())
      '())

(test "intersect"
      (intersect '(a b c) '(a x c))
      '(a c))

;; intersect: [set] [set] -> [set]
;; 1. revision of intersect
;; Page 37
(define intersect
  (lambda (set1 set2)
    (letrec
        ((I (lambda (s)
              (cond
               ((null? s) '())
               ((member? (car s) set2)
                (cons (car s) (I (cdr s))))
               (else (I (cdr s)))))))
      (I set1))))

(test "intersect - 1.revision"
      (intersect '(a b c) '(x b y))
      '(b))

(test "intersect - 1.revision"
      (intersect '() '(x b y))
      '())

(test "intersect - 1.revision"
      (intersect '(a b c) '())
      '())

(test "intersect - 1.revision"
      (intersect '(a b c) '(a x c))
      '(a c))

;; intersectall: (listof [set]) -> [set]
;; Page 38
(define intersectall
  (lambda (lset)
    (cond
     ((null? (cdr lset)) (car lset))
     (else (intersect (car lset) (intersectall (cdr lset)))))))

(test "intersectall"
      (intersectall '((a b c d e) (b c)  (a c)))
      '(c))

;; intersectall: (listof [set]) -> [set]
;; 1.revision of intersectall
;; Page 38
(define intersectall
  (lambda (lset)
    (cond
     ((null? lset) '())
     ((null? (cdr lset)) (car lset))
     (else (intersect (car lset) (intersectall (cdr lset)))))))

(test "intersectall - 1.revision"
      (intersectall '((a b c d e) (b c)  (a c)))
      '(c))

;; intersectall: (listof [set]) -> [set]
;; 2.revision of intersectall
;; Page 39
(define intersectall
  (lambda (lset)
    (letrec
        ((intersectall (lambda (lset)
                         (cond
                          ((null? (cdr lset)) (car lset))
                          (else (intersect (car lset)
                                           (intersectall (cdr lset))))))))
      (cond
       ((null? lset) '())
       (else (intersectall lset))))))

(test "intersectall - 2.revision"
      (intersectall '((a b c d e) (b c)  (a c)))
      '(c))

;; intersectall: (listof [set]) -> [set]
;; 3.revision of intersectall
;; Page 39
(define intersectall
  (lambda (lset)
    (letrec
        ((A (lambda (lset)
              (cond
               ((null? (cdr lset)) (car lset))
               (else (intersect (car lset)
                                (A (cdr lset))))))))
      (cond
       ((null? lset) '())
       (else (A lset))))))

(test "intersectall - 3.revision"
      (intersectall '((a b c d e) (b c)  (a c)))
      '(c))

;; intersectall: (listof [set]) -> [set]
;; 4.revision of intersectall
;; Page 41
(define intersectall
  (lambda (lset)
    (call/cc
     (lambda (hop)
       (letrec
           ((A (lambda (lset)
                 (cond
                  ((null? (car lset)) (hop '()))
                  ((null? (cdr lset)) (car lset))
                  (else (intersect (car lset)
                                   (A (cdr lset))))))))
         (cond
          ((null? lset) '())
          (else (A lset))))))))

(test "intersectall - 4.revision"
      (intersectall '((three mangoes and) ()  (three diet hamburgers)))
      '())

(test "intersectall - 4.revision"
      (intersectall '((three mangoes and) (three kiwis and)  (three hamburgers)))
      '(three))

(test "intersectall - 4.revision"
      (intersectall '((three mangoes and) (no food and) (three baked potatoes) (three diet hamburgers)))
      '())

;; intersect: [set] [set] -> [set]
;; 2. revision of intersect
;; Page 48
(define intersect
  (lambda (set1 set2)
    (letrec
        ((I (lambda (set1)
              (cond
               ((null? set1) '())
               ((member? (car set1) set2)
                (cons (car set1) (I (cdr set1))))
               (else (I (cdr set1)))))))
      (cond
       ((null? set2) '())
       (else (I set1))))))

(test "intersect - 2.revision"
      (intersect '(a b c) '(x b y))
      '(b))

(test "intersect - 2.revision"
      (intersect '() '(x b y))
      '())

(test "intersect - 2.revision"
      (intersect '(a b c) '())
      '())

(test "intersect - 2.revision"
      (intersect '(a b c) '(a x c))
      '(a c))

;; intersectall: (listof [set]) -> [set]
;; 5.revision of intersectall
;; Page 49
(define intersectall
  (lambda (lset)
    (call/cc
     (lambda (hop)
       (letrec
           ((A (lambda (lset)
                 (cond
                  ((null? (car lset)) (hop '()))
                  ((null? (cdr lset)) (car lset))
                  (else (I (car lset)
                           (A (cdr lset)))))))
            (I (lambda (s1 s2)
                 (letrec
                     ((J (lambda (s1)
                           (cond
                            ((null? s1) '())
                            ((member? (car s1) s2) (cons (car s1) (J (cdr s1))))
                            (else (J (cdr s1)))))))
                   (cond
                    ((null? s2) '())
                    (else (J s1)))))))
         (cond
          ((null? lset) '())
          (else (A lset))))))))

(test "intersectall - 5.revision"
      (intersectall '((three mangoes and) ()  (three diet hamburgers)))
      '())

(test "intersectall - 5.revision"
      (intersectall '((three mangoes and) (three kiwis and)  (three hamburgers)))
      '(three))

(test "intersectall - 5.revision"
      (intersectall '((three mangoes and) (no food and) (three baked potatoes) (three diet hamburgers)))
      '())

;; intersectall: (listof [set]) -> [set]
;; 6.revision of intersectall
;; Page 50
(define intersectall
  (lambda (lset)
    (call/cc
     (lambda (hop)
       (letrec
           ((A (lambda (lset)
                 (cond
                  ((null? (car lset)) (hop '()))
                  ((null? (cdr lset)) (car lset))
                  (else (I (car lset)
                           (A (cdr lset)))))))
            (I (lambda (s1 s2)
                 (letrec
                     ((J (lambda (s1)
                           (cond
                            ((null? s1) '())
                            ((member? (car s1) s2) (cons (car s1) (J (cdr s1))))
                            (else (J (cdr s1)))))))
                   (cond
                    ((null? s2) (hop '()))
                    (else (J s1)))))))
         (cond
          ((null? lset) '())
          (else (A lset))))))))

(test "intersectall - 6.revision"
      (intersectall '((three mangoes and) ()  (three diet hamburgers)))
      '())

(test "intersectall - 6.revision"
      (intersectall '((three mangoes and) (three kiwis and)  (three hamburgers)))
      '(three))

(test "intersectall - 6.revision"
      (intersectall '((three mangoes and) (no food and) (three baked potatoes) (three diet hamburgers)))
      '())

;; rember: [atom] [lat] -> [lat]
;; Page 52
(define rember
  (lambda (a lat)
    (letrec
        ((R (lambda (lat)
              (cond
               ((null? lat) '())
               ((eq? (car lat) a) (cdr lat))
               (else (cons (car lat) (R (cdr lat))))))))
      (R lat))))

(test "rember" (rember 'a '(a b c)) '(b c))
(test "rember" (rember 'a '(b a c)) '(b c))
(test "rember" (rember 'a '(c b a)) '(c b))
(test "rember" (rember 'a '(a b a)) '(b a))
(test "rember" (rember 'a '(x y z)) '(x y z))
(test "rember" (rember 'a '(b (a) c)) '(b (a) c))
(test "rember" (rember 'a '()) '())

;; rember-beyond-first: [atom] [lat] -> [lat]
;; Page 54
(define rember-beyond-first
  (lambda (a lat)
    (letrec
        ((R (lambda (lat)
              (cond
               ((null? lat) '())
               ((eq? (car lat) a) '())
               (else (cons (car lat) (R (cdr lat))))))))
      (R lat))))

(test "rember-beyond-first"
      (rember-beyond-first 'roots '(noodles spaghetti spaetzle bean-thread roots potatoes yam others rice))
      '(noodles spaghetti spaetzle bean-thread))

(test "rember-beyond-first"
      (rember-beyond-first 'others '(noodles spaghetti spaetzle bean-thread roots potatoes yam others rice))
      '(noodles spaghetti spaetzle bean-thread roots potatoes yam))

(test "rember-beyond-first"
      (rember-beyond-first 'sweething '(noodles spaghetti spaetzle bean-thread roots potatoes yam others rice))
      '(noodles spaghetti spaetzle bean-thread roots potatoes yam others rice))

(test "rember-beyond-first"
      (rember-beyond-first 'desserts '(cookies
                                       chocolate mints caramel delight ginger snaps desserts
                                       chocolate mousse
                                       vanilla ice cream
                                       German chocolate cake
                                       more desserts
                                       gingerbreadman chocolate chip brownies))
      '(cookies chocolate mints caramel delight ginger snaps))

;; rember-upto-last: [atom] [lat] -> [lat]
;; Page 57
(define rember-upto-last
  (lambda (a lat)
    (call/cc
     (lambda (skip)
       (letrec
           ((R (lambda (lat)
                 (cond
                  ((null? lat) '())
                  ((eq? (car lat) a) (skip (R (cdr lat))))
                  (else (cons (car lat) (R (cdr lat))))))))
         (R lat))))))

(test "rember-upto-last"
      (rember-upto-last 'roots '(noodles spaghetti spaetzle bean-thread roots potatoes yam others rice))
      '(potatoes yam others rice))

(test "rember-upto-last"
      (rember-upto-last 'others '(noodles spaghetti spaetzle bean-thread roots potatoes yam others rice))
      '(rice))

(test "rember-upto-last"
      (rember-upto-last 'sweething '(noodles spaghetti spaetzle bean-thread roots potatoes yam others rice))
      '(noodles spaghetti spaetzle bean-thread roots potatoes yam others rice))

(test "rember-upto-last"
      (rember-upto-last 'desserts '(cookies
                                       chocolate mints caramel delight ginger snaps desserts
                                       chocolate mousse
                                       vanilla ice cream
                                       German chocolate cake
                                       more desserts
                                       gingerbreadman chocolate chip brownies))
      '(gingerbreadman chocolate chip brownies))
