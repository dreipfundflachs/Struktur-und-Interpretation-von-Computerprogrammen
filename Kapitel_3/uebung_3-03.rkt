;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 3.03 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide konstr-konto kto)

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
((kto 'geheim 'abheben) 10)
((kto 'geheim 'einzahlen) 20)
((kto 'falsches-kennwort 'einzahlen) 20)
