;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 3.17 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide paare paar-zaehler x y z z3 z4 z7)

(define (paare sequenz gesehene)
  (if (or (not (pair? sequenz)) (memq sequenz gesehene))
    gesehene
    (paare (cdr sequenz)
           (paare (car sequenz) (cons sequenz gesehene)))))

(define (paar-zaehler sequenz)
  (length (paare sequenz '())))
                 
; Beispiele aus Übung 3.16:
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
(display z3)
(newline)
(display (paar-zaehler z3))
(newline)

; z4 = ((a b) b)
(define z4 (cons x (cdr x)))
(display z4)
(newline)
(display (paar-zaehler z4))
(newline)

; z7 = (z z) = (((a) a) (a) a)
(define z7 (cons z z))
(display z7)
(newline)
(display (paar-zaehler z7))
(newline)
