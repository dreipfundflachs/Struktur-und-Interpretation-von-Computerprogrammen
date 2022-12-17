;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.05 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide cons-1 car-1 cdr-1 extrahiere z a b)

(define (cons-1 a b) (* (expt 2 a) (expt 3 b)))

(define (car-1 z) (extrahiere z 2))

(define (cdr-1 z) (extrahiere z 3))

(define (extrahiere n basis)
  (if (= 0 (modulo n basis))
    (+ 1 (extrahiere (quotient n basis) basis))
    0))

; Beispiel:

(define z (cons-1 4 5))

(define a (car-1 z))

(define b (cdr-1 z))

; Beweis:
;
; Wir beweisen nur, dass (cdr (cons a b)) = b für alle positive ganze Zahlen
; gilt. Der Beweis, dass auch (car (cons a b)) = a gilt, ist ähnlich.
;
;   (cdr (cons a b))
;   {Auswertung des Rumpfes von 'cdr' und 'cons'}
; = (extrahiere (* (expt 2 a) (expt 3 b)) 3)
;   {Auswertung des ersten Arguments}
; = (extrahiere (2^a 3^b) 3)
;   {'extrahiere' anwenden}
; = (+ 1 (extrahiere 3 (2^a 3^(b - 1))))
;   {Anwendung von 'extrahiere'}
; .
; .
; .
; = (+ 1 (+ 1 (+ 1 ... (extrahiere (2^a) 3))))
;   {Anwendung von 'extrahiere'}
; = (+ 1 (+ 1 (+ 1 ...(+ 1 0))))
;   {Addiere 1 zu sich selbst b Male}
; = b
