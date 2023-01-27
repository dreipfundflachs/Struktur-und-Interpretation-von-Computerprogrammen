;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.48 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide konstr-strecke start-strecke ende-strecke u v w)

; Die Übung bittet um eine Implementation von Strecken,
; die die Vektordarstellung aus Übung 2.46 benutzt. Allerdings
; braucht man sie überhaupt nicht:

(define (konstr-strecke anfangspunkt endpunkt)
  (cons anfangspunkt endpunkt))

(define (start-strecke strecke) (car strecke))

(define (ende-strecke strecke) (cdr strecke))

; Implementation von Vektoren als Paar von Koordinaten:

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
