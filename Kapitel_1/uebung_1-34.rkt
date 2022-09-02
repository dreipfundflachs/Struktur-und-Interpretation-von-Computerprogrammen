;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.34 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; f ist gleich der Auswertungsfunktion an der Stelle 2: 
(define (f g)
  (g 2))

; Deshalb ergibt sich das Folgende:
; (f f) -> {Auswertung der Prozedur f auf der linken Seite}
; (f 2) -> {Auswertung von f}
; (2 2) -> {Fehler: '2' ist keine Prozedur}

; Beispiele

(define quadrat (lambda (x) (* x x)))

(define h (lambda (z) (* z (+ z 1))))
