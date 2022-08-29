;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.07 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (konstr-intervall a b)
  (cons a b))

(define (untere-grenze I) (car I))

(define (untere-grenze I) (cdr I))

(define (sub-intervall I J)
  (konstr-intervall
    (- (untere-grenze I)    (obere-grenze J))
    (- (obere-grenze I)     (untere-grenze J))))
