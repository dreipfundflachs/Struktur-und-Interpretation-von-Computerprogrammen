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
      (let [(temp (mcdr x))]
        (set-cdr! x y)
        (schleife temp x))))
  (schleife x '()))

(define v (mlist 'a 'b 'c 'd))

(define w (mysterioes v))

; `mysterioes` liefert als Ergebnis ein neues Objekt, das sein Argument in
; umgekehrter Reihenfolge gleicht. Das Argument selbst wird schon nach dem
; ersten Aufruf von `schleife` modifiziert, so dass sein cdr in Null
; umgewandelt wird.  Die folgende Aufrufe von `schleife` modifizieren das
; originelle Argument nicht mehr; deswegen gleicht as am Ende eine Liste, die
; nur aus dem originellen car besteht.
