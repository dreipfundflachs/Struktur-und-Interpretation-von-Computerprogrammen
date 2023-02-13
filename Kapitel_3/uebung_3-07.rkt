;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 3.07 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide konstr-gemeinsam konstr-konto)

(define (konstr-gemeinsam konto kennwort neues-kennwort)
  (begin ((konto kennwort 'hinzufuegen) neues-kennwort)
         konto))


(define (konstr-konto kontostand geheimes-kennwort)
  (let ([zaehler 0]
        [kennwoerter (list geheimes-kennwort)])
    (define (abheben betrag)
      (if (>= kontostand betrag)
        (begin (set! kontostand (- kontostand betrag))
               (set! zaehler 0)
               kontostand)
        (display "Deckung nicht ausreichend!\n")))
    (define (einzahlen betrag)
      (begin (set! kontostand (+ kontostand betrag))
             (set! zaehler 0)
             kontostand))
    (define (hinzufuegen kennwort)
      (begin (set! kennwoerter (cons kennwort kennwoerter))
             (set! zaehler 0)
             (display "Kennwort erfolgreich hinzugefügt!\n")))
    (define (nachricht betrag)
      (begin (set! zaehler (+ zaehler 1))
             (display "Falsches Kennwort!\n")))
    (define (zuteilen k n)
      (cond ([not (memq k kennwoerter)]
             (if (= zaehler 7)
               polizei-rufen
               nachricht))
            ([eq? n 'hinzufuegen] hinzufuegen)
            ([eq? n 'abheben] abheben)
            ([eq? n 'einzahlen] einzahlen)
            (else (error "Unbekannte Forderung -- KONSTR-KONTO" n))))
    zuteilen))

(define (polizei-rufen betrag) "Die Polizei kommt!")

; Beispiel:
(define kto (konstr-konto 100 'geheim))
((kto 'geheim 'abheben) 10)
((kto 'geheim 'einzahlen) 20)
((kto 'falsches-kennwort 'einzahlen) 20)
((kto 'geheim 'hinzufuegen) 'test)

(define peter-kto (konstr-konto 100 'sesam-oeffne-dich))
(define paul-kto (konstr-gemeinsam peter-kto 'sesam-oeffne-dich 'rosemarie))
((peter-kto 'sesam-oeffne-dich 'einzahlen) 20)
((paul-kto 'rosemarie 'einzahlen) 20)
((peter-kto 'rosemarie 'abheben) 20)
((paul-kto 'sesam-oeffne-dich 'abheben) 20)
((peter-kto 'falsches-kennwort 'hinzufuegen) 'a123)
(define andreas-kto (konstr-gemeinsam peter-kto 'falsches-kennwort 'a123))
