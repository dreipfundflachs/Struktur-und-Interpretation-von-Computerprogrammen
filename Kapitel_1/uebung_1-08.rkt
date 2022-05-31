;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.08 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (quadrat x) (* x x))

(define (kubus x) (* x x x))

(define (abs x)
  (if (> x 0) x (- x)))

; Berechnung von Kubikwurzeln nach dem Newtonschen Iterationsverfahren
(define (wurzel_iter schaetzwert x)
  (if (gut_genug? schaetzwert x)
    schaetzwert
    (wurzel_iter (verbessern schaetzwert x)
                 x)))

(define (verbessern schaetzwert x)
  (gewichteter_mittelwert schaetzwert x))

(define (gewichteter_mittelwert y x)
  (/ (+ (/ x (quadrat y)) (* 2 y)) 3))

(define (gut_genug? schaetzwert x)
  (< (abs (- (kubus schaetzwert) x))
     0.001))

(define (kubikwurzel x)
  (wurzel_iter 1.0 x))
