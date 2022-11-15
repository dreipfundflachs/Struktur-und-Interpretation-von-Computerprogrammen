;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.12 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide konstr-mittel-prozent mittel prozent
         konstr-intervall untere-grenze obere-grenze
         breite drucke-intervall I)

(define (konstr-mittel-prozent m p)
  (konstr-intervall (- m (* p m)) (+ m (* p m))))

(define (mittel I)
  (/ (+ (untere-grenze I) (obere-grenze I)) 2))

(define (prozent I)
  (/ (breite I) (mittel I)))

(define (konstr-intervall a b)
  (cons a b))

(define (untere-grenze I) (car I))

(define (obere-grenze I) (cdr I))

(define (breite I)
  (/ (- (obere-grenze I) (untere-grenze I)) 2))

(define (drucke-intervall I)
  (display "[")
  (display (untere-grenze I))
  (display " , ")
  (display (obere-grenze I))
  (display "]"))

; Beispiel:

(define I (konstr-mittel-prozent 3.5 0.15))
