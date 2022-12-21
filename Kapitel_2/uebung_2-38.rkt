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

; Um zu garantieren, dass fold-left und fold-right für jede Sequenz dieselben
; Werte liefern, muss die Kombinierungsoperation 'op' assoziativ sein, d.h.,
; sie muss folgende Bedingung für alle a, b und c erfüllen:
;   (op (op a b) c) = (op a (op b c))

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
