;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 3.06 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide zufall zufall-init zufall-aktuell z)

(define zufall-init 0)

(define (zufall-aktuell x)
  (+ x 1))

(define (zufall n)
  (let ([x zufall-init])
    (define (generieren)
      (begin (set! x (zufall-aktuell x))
             x))
    (define (zuruecksetzen neuer-wert)
      (begin (set! x neuer-wert)
             x))
    (define (zuteilen nachricht)
      (cond ([eq? nachricht 'generieren] (generieren))
            ([eq? nachricht 'zuruecksetzen] zuruecksetzen)
            (else (error "Unbekannte Forderung -- ZUFALL"))))
    (zuteilen n)))

(define z zufall)
    
