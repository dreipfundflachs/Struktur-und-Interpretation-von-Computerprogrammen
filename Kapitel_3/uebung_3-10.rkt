;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 3.10 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide konstr-abheben neues-konstr-abheben)

(define (konstr-abheben kontostand)
  (lambda (betrag)
    (if (>= kontostand betrag)
      (begin (set! kontostand (- kontostand betrag))
             kontostand)
      "Deckung nicht ausreichend!")))

(define (neues-konstr-abheben anfangs-betrag)
  (let [(kontostand anfangs-betrag)]
    (lambda (betrag)
      (if (>= kontostand betrag)
        (begin (set! kontostand (- kontostand betrag))
               kontostand)
        "Deckung nicht ausreichend!"))))

(define W1 (neues-konstr-abheben 100))

(W1 50)

(define W2 (neues-konstr-abheben 100))

(W2 20)

