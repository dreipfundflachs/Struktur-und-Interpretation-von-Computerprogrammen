;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.47 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide konstr-rahmen1 konstr-rahmen2
         ursprung-rahmen
         kante1-rahmen
         kante2-rahmen2 kante2-rahmen1
         rahmen1 rahmen2)

(define (konstr-rahmen1 ursprung kante1 kante2)
  (list ursprung kante1 kante2))

(define (konstr-rahmen2 ursprung kante1 kante2)
  (cons ursprung (cons kante1 kante2)))

(define (ursprung-rahmen rahmen)
  (car rahmen))

(define (kante1-rahmen rahmen)
  (cadr rahmen))

(define (kante2-rahmen1 rahmen)
  (caddr rahmen))

(define (kante2-rahmen2 rahmen)
  (cddr rahmen))

; Beispiele:

(define (konstr-vekt x y) (cons x y))

(define u (konstr-vekt 1 2))

(define v (konstr-vekt 3 (- 4)))

(define w (konstr-vekt (- 1) 5))

(define rahmen1 (konstr-rahmen1 u v w))

(define rahmen2 (konstr-rahmen2 u v w))
