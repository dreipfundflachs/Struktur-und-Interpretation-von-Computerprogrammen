;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  SICP - Abschnitt 3.1.1  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide abheben neu-abheben konstr-abheben W1 W2
         konstr-konto kto kto2)

(define kontostand 100)

(define (abheben betrag)
  (if (>= kontostand betrag)
    (begin (set! kontostand (- kontostand betrag))
           kontostand)
    "Deckung nicht ausreichend"))

(define neu-abheben
  (let ([kontostand 100])
    (lambda (betrag)
      (if (>= kontostand betrag)
        (begin (set! kontostand (- kontostand betrag))
               kontostand)
        "Deckung nicht ausreichend"))))

(define (konstr-abheben kontostand)
    (lambda (betrag)
      (if (>= kontostand betrag)
        (begin (set! kontostand (- kontostand betrag))
               kontostand)
        "Deckung nicht ausreichend")))

(define W1 (konstr-abheben 100))
(define W2 (konstr-abheben 100))

(define (konstr-konto kontostand)
  (define (abheben betrag)
    (if (>= kontostand betrag)
      (begin (set! kontostand (- kontostand betrag))
             kontostand)
      "Deckung nicht ausreichend"))
  (define (einzahlen betrag)
    (set! kontostand (+ kontostand betrag))
    kontostand)
  (define (zuteilen n)
    (cond ([eq? n 'abheben] abheben)
          ([eq? n 'einzahlen] einzahlen)
          (else (error "Unbekannte Forderung -- KONSTR-KONTO" n))))
  zuteilen)

(define kto (konstr-konto 100))
(define kto2 (konstr-konto 100))
