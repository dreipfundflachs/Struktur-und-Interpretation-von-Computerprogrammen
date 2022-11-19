;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.34 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide akkumuliere horner-schema koeffizienten)

(define (akkumuliere op anfangswert sequenz)
  (if (null? sequenz)
    anfangswert
    (op (car sequenz) (akkumuliere op anfangswert (cdr sequenz)))))

(define (horner-schema x koeffizienten-sequenz)
    (akkumuliere (lambda (koeff hoehere-terme) (+ koeff (* x hoehere-terme)))
                 0
                 koeffizienten-sequenz))

; Koeffizienten des Polynoms 1 + 3x + 5x^3 + x^5:
(define koeffizienten (list 1 3 0 5 0 1))
