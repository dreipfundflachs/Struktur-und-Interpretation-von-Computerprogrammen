;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 3.14 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(provide mysterioes v w)

(define (mysterioes x)
  (define (schleife x y)
    (if (null? x)
      y
      (let [(temp (cdr x))]
        (set-cdr! x y)
        (schleife temp x))))
  (schleife x '()))

(define v (mlist 'a 'b 'c 'd))

(define w (mysterioes v))
