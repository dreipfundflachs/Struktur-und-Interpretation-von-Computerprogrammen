;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 3.29 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(define (oder-gatter o1 o2 ausgabe)
  (let [(a1 (konstr-draht))
        (a2 (konstr-draht))
        (b (konstr-draht))]
    (inverter o1 a1)
    (inverter o2 a2)
    (und-gatter a1 a2 b)
    (inverter b ausgabe))
  'ok!)

; Die Verzögerungszeit des ODER-Gatters ist die Summe der Verzögerung
; des UND-Gatters und der doppelten Verzögerung des Inverters.
