;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.06 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide zero addiere-1 eins eins-1 zwei plus)

(define zero (lambda (f) (lambda (x) x)))

(define (addiere-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define eins
  (lambda (f) (lambda (x) (f x))))

; Das heisst, eins: f -> f. Deshalb können wir 'eins' auch wie folgt definieren:

(define eins-1
  (lambda (f) f))

(define zwei
  (lambda (f) (lambda (x) (f (f x)))))

(define (plus m n)
  (lambda (f) (m (n f))))
