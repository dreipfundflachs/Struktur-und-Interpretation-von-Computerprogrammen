;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.07 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (quadrat x) (* x x))

(define (abs x)
  (if (> x 0) x (- x)))

(define (mittelwert x y)
  (/ (+ x y) 2))

; Verbesserte Berechnung von Quadratwurzeln nach dem Newtonschen
; Iterationsverfahren
(define (wurzel_iter schaetzwert x)
  (if (gut_genug? schaetzwert x)
    schaetzwert
    (wurzel_iter (verbessern schaetzwert x)
                 x)))

(define (verbessern schaetzwert x)
  (mittelwert schaetzwert (/ x schaetzwert)))

(define (gut_genug? schaetzwert x)
  (nah_genug? schaetzwert (verbessern schaetzwert x)))

(define (nah_genug? x y)
  (< (abs (- (/ x y) 1)) 0.0001))

(define (wurzel x)
  (wurzel_iter 1.0 x))
