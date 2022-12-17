;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.15 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Kurz gesagt, das Problem stammt von der Tatsache, dass A / A /= 1.0 ist,
; mit Ausnahme vom Fall in dem A nur aus einem einzigen Punkt besteht. Deshalb,
; obwohl die zwei Ausdrücke
;   R_1 R_2 / (R_1 + R_2)       und
;   1 / (1 / R_1 + 1 / R_2)
; äquivalent sind, wenn R_1 und R_2 Zahlen sind (oder, allgemeneiner, wenn sie
; Elemente eines Körpers sind), sind sie nicht äquivalent, wenn R_1 und R_2
; Intervalle sind.
