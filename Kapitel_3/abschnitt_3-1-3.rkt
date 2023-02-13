;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  SICP - Abschnitt 3.1.3  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide konstr-vereinfachtes-abheben konstr-dekrementierer)

(define (konstr-vereinfachtes-abheben kontostand)
  (lambda (betrag)
    (set! kontostand (- kontostand betrag))
    kontostand))

(define W (konstr-vereinfachtes-abheben 25))

(W 20)
(W 10)

(define (konstr-dekrementierer kontostand)
  (lambda (betrag)
    (- kontostand betrag)))

(define D (konstr-dekrementierer 25))

(D 20)
(D 10)

(define D1 (konstr-dekrementierer 25))
(define D2 (konstr-dekrementierer 25))

(define W1 (konstr-vereinfachtes-abheben 25))
(define W2 (konstr-vereinfachtes-abheben 25))
