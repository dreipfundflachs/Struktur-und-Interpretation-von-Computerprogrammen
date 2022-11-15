;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.08 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide konstr-intervall untere-grenze obere-grenze sub-intervall
         I J K L)

(define (konstr-intervall a b)
  (cons a b))

(define (untere-grenze I) (car I))

(define (obere-grenze I) (cdr I))

; Der kleinste mögliche Wert der Differenz I - J zweier Intervallen I und J ist
; die Differenz der unteren Grenze von I und der oberen Grenze von J. Der
; größte mögliche Wert der Differenz I - J ist die Differenz der oberen Grenze
; von I und der unteren Grenze von J.

(define (sub-intervall I J)
  (konstr-intervall
    (- (untere-grenze I)    (obere-grenze J))
    (- (obere-grenze I)     (untere-grenze J))))

; Beispiele von Intervallen:

; I = [1, 3]
(define I (konstr-intervall 1 3))
 
; J = [2, 4]
(define J (konstr-intervall 2 4))

; K = [-1, 1]
(define K (konstr-intervall (- 1) 1))

; L = [-3, -1]
(define L (konstr-intervall (- 3) (- 1)))
