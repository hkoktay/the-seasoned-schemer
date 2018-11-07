;; chapter 15: The Difference Between Men and Boys

(load "test-check.scm")

(define x
  (cons 'chicago
    (cons 'pizza '())))

(test "x" x '(chicago pizza))

(define x 'gone)

(test "x" x 'gone)

(set! x 'skins)

(test "x" x 'skins)

;; gourmet: any -> pair
;; Page 92
(define gourmet
  (lambda (food)
    (cons food (cons x '()))))

(test "gourmet"
      (gourmet 'onion)
      '(onion skins))

(set! x 'rings)

;; gourmand: any -> pair
;; Page 93
(define gourmand
  (lambda (food)
    (set! x food)
    (cons food (cons x '()))))

(test "gourmand"
      (gourmand 'potato)
      '(potato potato))

;; diner: any -> pair
;; Page 94
(define diner
  (lambda (food)
    (cons 'milkshake (cons food '()))))

(test "diner"
      (diner 'milkshake)
      '(milkshake milkshake))

;; dinerR: any -> pair
;; Page 94
(define dinerR
  (lambda (food)
    (set! x food)
    (cons 'milkshake (cons food '()))))

(test "dinerR"
      (dinerR 'onion)
      '(milkshake onion))

(test "dinerR"
      (dinerR 'pecanpie)
      '(milkshake pecanpie))

(test "gourmand"
      (gourmand 'onion)
      '(onion onion))

;; omnivore: any -> pair
;; Page 96
(define omnivore
  (let ((x 'minestrone))
    (lambda (food)
      (set! x food)
      (cons food (cons x '())))))

(test "omnivore"
      (omnivore 'onion)
      '(onion onion))

;; gobbler: any -> pair
;; Page 98
(define gobbler
  (let ((x 'minestrone))
    (lambda (food)
      (set! x food)
      (cons food (cons x '())))))

(test "gobbler"
      (gobbler 'gumbo)
      '(gumbo gumbo))

;; nibbler: any -> pair
;; 100
(define nibbler
  (lambda (food)
    (let ((x 'donut))
      (set! x food)
      (cons food (cons x '())))))

(test "nibbler"
      (nibbler 'cheerio)
      '(cheerio cheerio))

;; Page 102
(define food 'none)

;; glutton: any -> pair
;; Page 102
(define glutton
  (lambda (x)
    (set! food x)
    (cons 'more
      (cons x
        (cons 'more
          (cons x '()))))))

(test "glutton"
      (glutton 'garlic)
      '(more garlic more garlic))

;; chez-nous: void -> void
;; Page 103
(define chez-nous
  (lambda ()
    (set! food x)
    (set! x food)))

(chez-nous)
(test "food" food 'onion)
(test "x" x 'onion)

;; chez-nous: void -> void
;; 1.revison of chez-nous
;; Page 104
(define chez-nous
  (lambda ()
    (let ((a food))
      (set! food x)
      (set! x a))))

(test "glutton"
      (glutton 'garlic)
      '(more garlic more garlic))

(test "food" food 'garlic)

(test "gourmand"
      (gourmand 'potato)
      '(potato potato))

(test "x" x 'potato)

(chez-nous)
;; The values of the variables are swapped.
(test "food" food 'potato)
(test "x" x 'garlic)
