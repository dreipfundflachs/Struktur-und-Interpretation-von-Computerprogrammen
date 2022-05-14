;;;;;;;;;;;;;;;;;;;;;;;;;;
;  SICP - Abschnitt 1.1  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (quadrat x) (* x x))

(define (quadrat_summe x y) (+ (quadrat x) (quadrat y)))

(define (f a)
  (quadrat_summe (+ a 1) (* a 2)))

#|
(define (abs x)
  (cond (> x 0) x
        (else x)))
|#

(define (abs x)
  (if (> x 0) x (- x)))


; Berechnung von Quadratwurzeln nach dem Newtonschen Iterationsverfahren
(define (wurzel_iter schaetzwert x)
  (if (gut_genug? schaetzwert x)
    schaetzwert
    (wurzel_iter (verbessern schaetzwert x)
                 x)))

(define (verbessern schaetzwert x)
  (mittelwert schaetzwert (/ x schaetzwert)))

(define (mittelwert x y)
  (/ (+ x y) 2))

(define (gut_genug? schaetzwert x)
  (< (abs (- (quadrat schaetzwert) x))
     0.001))

(define (wurzel x)
  (wurzel_iter 1.0 x))
