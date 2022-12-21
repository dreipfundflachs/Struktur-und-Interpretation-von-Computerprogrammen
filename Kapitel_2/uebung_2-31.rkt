;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.31 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide baum baum-abb quadrat kubik quadrat-baum)

(define baum (list 0 (list 1 2) (list 3 4) 5))

(define (baum-abb proz baum)
  (cond ((null? baum) null)
        ((not (pair? baum)) (proz baum))
        (else (cons (baum-abb proz (car baum))
                    (baum-abb proz (cdr baum))))))

; Beispiel:

(define (quadrat x) (* x x))

(define (kubik x) (* x x x))

(define quadrat-baum (lambda (baum) (baum-abb quadrat baum)))
