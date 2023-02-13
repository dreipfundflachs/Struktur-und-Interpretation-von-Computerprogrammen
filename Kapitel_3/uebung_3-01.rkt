;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 3.01 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide konstr-akkumulator A B)

(define (konstr-akkumulator anfangswert)
  (let ([summe anfangswert])
    (lambda (term)
      (begin (set! summe (+ summe term))
             summe))))

(define A (konstr-akkumulator 10))
(define B (konstr-akkumulator 10))
