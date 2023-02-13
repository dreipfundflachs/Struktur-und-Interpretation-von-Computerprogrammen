;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 3.04 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide konstr-konto kto)

(define (polizei-rufen betrag) "Die Polizei kommt!")

(define (konstr-konto kontostand geheimes-kennwort)
  (let ([zaehler 0])
    (define (abheben betrag)
      (if (>= kontostand betrag)
        (begin (set! kontostand (- kontostand betrag))
               (set! zaehler 0)
               kontostand)
        "Deckung nicht ausreichend"))
    (define (einzahlen betrag)
      (begin (set! kontostand (+ kontostand betrag))
             (set! zaehler 0)
             kontostand))
    (define (nachricht betrag)
      (begin (set! zaehler (+ zaehler 1))
             "Falsches Kennwort!"))
    (define (zuteilen k n)
      (cond ([not (eq? k geheimes-kennwort)]
             (if (= zaehler 7)
               polizei-rufen
               nachricht))
            ([eq? n 'abheben] abheben)
            ([eq? n 'einzahlen] einzahlen)
            (else (error "Unbekannte Forderung -- KONSTR-KONTO" n))))
    zuteilen))

(define kto (konstr-konto 100 'geheim))
((kto 'geheim 'abheben) 10)
((kto 'geheim 'einzahlen) 20)
((kto 'falsches-kennwort 'einzahlen) 20)
((kto 'falsches-kennwort 'einzahlen) 20)
((kto 'falsches-kennwort 'einzahlen) 20)
((kto 'falsches-kennwort 'einzahlen) 20)
((kto 'falsches-kennwort 'einzahlen) 20)
((kto 'falsches-kennwort 'einzahlen) 20)
((kto 'falsches-kennwort 'einzahlen) 20)
((kto 'falsches-kennwort 'einzahlen) 20)
