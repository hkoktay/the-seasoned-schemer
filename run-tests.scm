#! /usr/local/bin/scheme --program

(import (chezscheme))

;; load-test: string? -> void
;; 
;; We need this procedure because we have to reset the interaction environment
;; before loading another source file to prevent unexpected test results. The
;; reason is that we revise and thus redefine some procedures in later chapters.
(define load-test
  (lambda (t)
    (begin
      (interaction-environment (copy-environment (scheme-environment) #t))
      (load t))))

;; load-and-test: listof-string? -> void
(define load-and-test
  (lambda (l)
    (for-each (lambda (f) (load-test f)) l)))

(load-and-test
 '("chapter11.scm"
   "chapter12.scm"
   "chapter13.scm"
   "chapter14.scm"
   "chapter15.scm"
   "chapter16.scm"
   "chapter17.scm"
   "chapter18.scm"
   "chapter19.scm"
   "chapter20.scm"))