;;;;;;;;;;;;;;;;;;;;;;;;;;
;  SICP - Abschnitt 2.1  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide cons1 car1 cdr1)

(define (cons1 x y)
  (define (zuteilen m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument nicht 0 oder 1 -- CONS" m))))
  zuteilen)

(define (car1 z) (z 0))

(define (cdr1 z) (z 1))

; Beispiel:

(define z (cons1 0 1))

z

(car1 z)

(cdr1 z)
