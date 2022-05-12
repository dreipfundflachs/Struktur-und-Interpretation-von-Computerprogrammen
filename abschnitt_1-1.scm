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
