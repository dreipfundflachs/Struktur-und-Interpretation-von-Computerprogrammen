;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.04 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide cons-1 car-1 cdr-1)

(define (cons-1 x y)
  (lambda (m) (m x y)))

(define (car-1 z)
  (z (lambda (p q) p)))

(define (cdr-1 z)
  (z (lambda (p q) q)))

; Zum Beweis, dass car (cons x y) = x ist:
;
; 1. (car (cons x y))    {Auswertung des Rumpfes von 'car'}
; 2. = ((cons x y) (lambda (p q) p))    {Auswertung von (cons x y)}
; 3. = ((lambda (m) (m x y)) (lambda (p q) p))
;    {Anwendung der ersten Prozedur auf ihr Argument}
; 4. = ((lambda (p q) p) x y)
;    {Auswertung der Prozedur}
; 5. = x


; Analog kann man leicht beweisen, dass (cdr (cons x y)) = y ist.
