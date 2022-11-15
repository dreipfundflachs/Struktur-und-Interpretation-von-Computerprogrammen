;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.10 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide add-intervall kehr-intervall div-intervall drucke-intervall
         konstr-intervall untere-grenze obere-grenze
         I J K)

(define (konstr-intervall a b)
  (cons a b))

(define (untere-grenze I) (car I))

(define (obere-grenze I) (cdr I))

(define (kehr-intervall I)
  (if (and (<= (untere-grenze J) 0) (>= (obere-grenze J) 0))
  (error "Division durch ein Intervall, das Null enthält, ist nicht erlaubt!")
  (konstr-intervall (/ 1.0 (obere-grenze I))
                    (/ 1.0 (untere-grenze I)))))

(define (div-intervall I J)
  (mul-intervall I (kehr-intervall J)))

(define (add-intervall I J)
  (konstr-intervall (+ (untere-grenze I) (untere-grenze J))
                    (+ (obere-grenze I) (obere-grenze J))))

(define (mul-intervall I J)
  (let ((p1 (* (untere-grenze I)    (untere-grenze J)))
        (p2 (* (untere-grenze I)    (obere-grenze J)))
        (p3 (* (obere-grenze I)     (untere-grenze J)))
        (p4 (* (obere-grenze I)     (obere-grenze J))))
    (konstr-intervall (min p1 p2 p3 p4)
                      (max p1 p2 p3 p4))))

(define (drucke-intervall I)
  (display "[")
  (display (untere-grenze I))
  (display " , ")
  (display (obere-grenze I))
  (display "]"))

; Beispiele von Intervallen:

; I = [1, 2]
(define I (konstr-intervall 1 2))
 
; J = [-1 1]
(define J (konstr-intervall (- 1) 1))

; K = [4, 5]
(define K (konstr-intervall 4 5))
