;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.75 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide konstr-aus-abs-wkl anwenden-generisch)

(define (konstr-aus-abs-wkl r a)
  (define (zuteilen op)
    (cond ([eq? op 'reeller-teil] (* r (cos a)))
          ([eq? op 'imag-teil] (* r (sin a)))
          ([eq? op 'abs-wert] r)
          ([eq? op 'winkel] a)
          (else
            (error "Unbekannte Operation -- KONSTR-AUS-REEL-IMAG" op))))
  zuteilen)

(define (anwenden-generisch op arg) (arg op))
