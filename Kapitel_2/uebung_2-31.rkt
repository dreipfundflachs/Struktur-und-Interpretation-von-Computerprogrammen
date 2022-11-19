;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.31 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide baum abb-baum quadrat kubik quadrat-baum)

(define baum (list 0 (list 1 2) (list 3 4) 5))

(define (abb-baum proz baum)
  (cond ((null? baum) null)
        ((not (pair? baum)) (proz baum))
        (else (cons (abb-baum proz (car baum))
                    (abb-baum proz (cdr baum))))))

; Beispiel:

(define (quadrat x) (* x x))

(define (kubik x) (* x x x))

(define quadrat-baum (lambda (baum) (abb-baum quadrat baum)))
