;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 3.11 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide konstr-konto kto kto2)

(define (konstr-konto kontostand)
  (define (abheben betrag)
    (if (>= kontostand betrag)
      (begin (set! kontostand (- kontostand betrag))
             kontostand)
      "Deckung nicht ausreichend!"))
  (define (einzahlen betrag)
    (set! kontostand (+ kontostand betrag))
    kontostand)
  (define (zuteilen n)
    (cond [(eq? n 'abheben) abheben]
          [(eq? n 'einzahlen) einzahlen]
          [else (error "Unbekannte Forderung -- KONSTR-KONTO" n)]))
  zuteilen)

(define kto (konstr-konto 50))

((kto 'einzahlen) 40)
; 90

((kto 'abheben) 60)
; 30

(define kto2 (konstr-konto 100))

; Die lokalen Zustände für die beiden Konten werden in zwei verschiedenen
; Umgebungen auseinandergehalten. Diese zwei Umgebungen teilen nur die globale
; Umgebung mit einander.
