;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 3.03 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide konstr-konto kto kto2)

(define (konstr-konto kontostand geheimes-kennwort)
  (define (abheben betrag)
    (if (>= kontostand betrag)
      (begin (set! kontostand (- kontostand betrag))
             kontostand)
      "Deckung nicht ausreichend"))
  (define (einzahlen betrag)
    (begin (set! kontostand (+ kontostand betrag))
           kontostand))
  (define (nachricht betrag) "Falsches Kennwort!")
  (define (zuteilen k n)
    (cond ([not (eq? k geheimes-kennwort)] nachricht)
          ([eq? n 'abheben] abheben)
          ([eq? n 'einzahlen] einzahlen)
          (else (error "Unbekannte Forderung -- KONSTR-KONTO" n))))
  zuteilen)

(define kto (konstr-konto 100 'geheim))
(define kto2 (konstr-konto 100 'abc123))
