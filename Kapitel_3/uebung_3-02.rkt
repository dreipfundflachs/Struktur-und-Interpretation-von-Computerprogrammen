;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 3.02 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide konstr-monitor quadrat mquadrat)

(define (konstr-monitor f)
  (let ([zaehler 0])
    (lambda (x)
      (cond ([eq? x 'wie-oft-aufgerufen?] zaehler)
            ([eq? x 'zaehler-zuruecksetzen] (set! zaehler 0))
            (else (begin (set! zaehler (+ 1 zaehler))
                         (f x)))))))

(define (quadrat x) (* x x))

(define mquadrat (konstr-monitor quadrat))
