;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.07 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide konstr-intervall untere-grenze obere-grenze
         I J K L)

(define (konstr-intervall a b)
  (cons a b))

(define (untere-grenze I) (car I))

(define (obere-grenze I) (cdr I))

; Beispiele von Intervallen:

; I = [1, 3]
(define I (konstr-intervall 1 3))
 
; J = [2, 4]
(define J (konstr-intervall 2 4))

; K = [-1, 1]
(define K (konstr-intervall (- 1) 1))

; L = [-3, -1]
(define L (konstr-intervall (- 3) (- 1)))
