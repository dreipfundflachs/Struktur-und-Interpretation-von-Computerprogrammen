;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 3.13 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(provide konstr-ring
         letztes-paar
         x z)

(define (letztes-paar x)
  (if (null? (mcdr x))
    x
    (letztes-paar (mcdr x))))

(define (konstr-ring x)
  (set-cdr! (letztes-paar x) x)
  x)

(define x (mlist 'a 'b 'c))

(define z (konstr-ring (mlist 'a 'b 'c)))

; Wenn wir (letztes-parr z) zu berechnen versuchen, würde sich eine eindlose
; Schleife ergeben, denn `letztes-paar` würde nie das Ende von z erreichen. In
; der Tat, z enthält den Nullelement nicht.
