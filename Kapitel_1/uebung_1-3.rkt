;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.03 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (quadrat_summe_groesser x y z)
  (cond ((= x (min x y z))  (quadrat_summe y z))
        ((= y (min x y z))  (quadrat_summe x z))
        (else               (quadrat_summe x y))))

(define (quadrat x) (* x x))

(define (quadrat_summe x y)
  (+ (quadrat x) (quadrat y)))

#|
(define (<= x y) (not (> x y)))

(define (min x y z)
  (cond ((and (<= x y) (<= x z)) x)
        ((and (<= y x) (<= y z)) y)
        (else z)))
|#
