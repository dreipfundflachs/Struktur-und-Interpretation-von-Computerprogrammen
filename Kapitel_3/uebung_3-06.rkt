;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 3.06 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide zufall zufall-init zufall-aktualisieren)

(define zufall-init 0)

(define (zufall-aktualisieren x) (+ x 1))

(define zufall
  (let ([x zufall-init])
    (define (zuteilen nachricht)
      (cond ([eq? nachricht 'generieren]
             (begin (set! x (zufall-aktualisieren x))
                    x))
            ([eq? nachricht 'zuruecksetzen]
             (lambda (neuer-wert)
               (begin (set! x neuer-wert)
                      x)))
            (else (error "Unbekannte Forderung -- ZUFALL"))))
    zuteilen))
