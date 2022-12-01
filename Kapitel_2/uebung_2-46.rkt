;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.46 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide konstr-vekt xkoord-vekt ykoord-vekt
         add-vekt sub-vekt skaliere-vekt
         drucke-vekt
         u v w c)

(define (konstr-vekt x y) (cons x y))

(define (xkoord-vekt v) (car v))

(define (ykoord-vekt v) (cdr v))

(define (add-vekt u v)
  (konstr-vekt
    (+ (xkoord-vekt u) (xkoord-vekt v))
    (+ (ykoord-vekt u) (ykoord-vekt v))))

(define (sub-vekt u v)
  (konstr-vekt
    (- (xkoord-vekt u) (xkoord-vekt v))
    (- (ykoord-vekt u) (ykoord-vekt v))))

(define (skaliere-vekt c v)
  (konstr-vekt
    (* c (xkoord-vekt v))
    (* c (ykoord-vekt v))))

(define (drucke-vekt v)
  (display "(")
  (display (xkoord-vekt v))
  (display " , ")
  (display (ykoord-vekt v))
  (display ")"))

; Beispiele:

(define u (konstr-vekt 1 2))

(define v (konstr-vekt 3 (- 4)))

(define w (konstr-vekt (- 1) 5))

(define c 10)
