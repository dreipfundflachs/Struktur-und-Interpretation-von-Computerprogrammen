;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.21 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide quadrat-liste quadrat-liste1 quadrat-liste2 zahlen)

(define (quadrat-liste elemente)
  (if (null? elemente)
    null
    (cons ((lambda (x) (* x x)) (car elemente))
          (quadrat-liste (cdr elemente)))))

; 'abb' ist in Scheme als 'map' vordefiniert.
(define (quadrat-liste1 elemente)
  (map (lambda (x) (* x x)) elemente))

(define quadrat-liste2
  (lambda (zahlen) (map (lambda (x) (* x x)) zahlen)))

(define zahlen (list 0 1 2 3 4 5 6 7 8 9 10))
