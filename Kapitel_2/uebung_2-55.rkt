;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.55 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(car ''abracadabra)

(car (quote (quote abracadabra)))

; (car ''abracadabra) ist gleich
; (car (quote (quote abracadabra)))
; Nun ist
; (quote (quote abracadabra))
; eine Liste die zwei Elemente enthält: 'quote und 'abracadabra,
; deshalb ist die Antwort der Interpretierer 'quote.
