;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 3.15 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(define x (mlist 'a 'b))

(define z1 (mcons x x))

(define z2 (mcons (mlist 'a 'b) (mlist 'a 'b)))

(define (set-wow! x)
  (set-car! (mcar x) 'wow)
  (display x)
  (newline))

(display z1)
(newline)

(set-wow! z1)

(display z2)
(newline)

(set-wow! z2)
