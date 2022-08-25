;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.21 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (kleinster-teil n)
  (finde-teiler n 2))

(define (finde-teiler n pruef-teiler)
  (cond ((> (quadrat pruef-teiler) n) n)
        ((teilt? pruef-teiler n) pruef-teiler)
        (else (finde-teiler n (+ pruef-teiler 1)))
  ))

(define (quadrat x) (* x x))

(define (teilt? a b)
  (= (remainder b a) 0))

; (kleinster-teil 199) -> 199
; (kleinster-teil 1999) -> 1999
; (kleinster-teil 19999) -> 7
