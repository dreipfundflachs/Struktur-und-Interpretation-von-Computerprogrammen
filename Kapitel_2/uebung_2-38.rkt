;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.38 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide fold-left fold-right)

(define (fold-left op ergebnis sequenz)
  (if (null? sequenz)
    ergebnis
    (fold-left op (op ergebnis (car sequenz)) (cdr sequenz))))

(define (fold-right op anfangswert sequenz)
  (if (null? sequenz)
    anfangswert
    (op (car sequenz) (fold-right op anfangswert (cdr sequenz)))))

; Beispiele:

(fold-right / 1 (list 1 2 3))
; (1 / (2 / (3 / 1)) = 3 / 2

(fold-left / 1 (list 1 2 3))
; (((1 / 1) / 2) / 3) = 1 / 6

(fold-right list null (list 1 2 3))
;   (list 1 (list 2 (list 3 null))
; = (1 (2 (3 ())))

(fold-left list null (list 1 2 3))
; (((() 1) 2) 3)
