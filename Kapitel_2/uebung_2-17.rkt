;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.17 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide letztes-paar letztes-paar1 gerade)

(define (letztes-paar elemente)
  (list (list-ref elemente (- (length elemente) 1))))

(define (letztes-paar1 elemente)
  (cond [(null? elemente) null]
        [(null? (cdr elemente)) elemente]
        [else (letztes-paar1 (cdr elemente))]))

(define gerade (list 0 2 4 6 8 10))
