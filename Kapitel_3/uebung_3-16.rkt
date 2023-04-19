;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 3.16 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(provide paar-zaehler x z3 z4 z7 zu)

(define (paar-zaehler x)
  (if (not (pair? x))
    0
    (+ (paar-zaehler (car x))
       (paar-zaehler (cdr x))
       1)))

; Beispiele:
; x = (a b)
(define x '(a b))
(display x)
(newline)
(display (paar-zaehler x))
(newline)

; y = (a)
(define y '(a))
(display y)
(newline)
(display (paar-zaehler y))
(newline)

; z = ((a) a)
(define z (cons y y))
(display z)
(newline)
(display (paar-zaehler z))
(newline)

; z3 = (a b c)
(define z3 '(a b c))
; `paar-zaehler` liefert 3 als Ergebnis, und in der Tat besteht diese Liste
; als zwei Paaren:
; z3 = (cons 'a (cons 'b (cons 'c null))) -> 3 cons Strukturen!
(display z3)
(newline)
(display (paar-zaehler z3))
(newline)

; z4 = ((a b) b)
(define z4 (cons x (cdr x)))
(display z4)
(newline)
; `paar-zaehler` liefert 4 als Ergebnis, obwohl nur drei Paaren vorhanden sind:
(display (paar-zaehler z4))
(newline)

; z7 = (z z) = (((a) a) (a) a)
(define z7 (cons z z))
(display z7)
(newline)
; `paar-zaehler` liefert 7 als Ergebnis, obwohl nur drei Paaren vorhanden sind:
(display (paar-zaehler z7))
(newline)

; `paar-zaehler` liefert gar kein Ergebnis:
(define zu (mlist 'a))
(set-cdr! zu zu)
