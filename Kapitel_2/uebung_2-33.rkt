;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.33 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide akkumuliere abb append1 length1 x y kubik)

(define (akkumuliere op anfangswert sequenz)
  (if (null? sequenz)
    anfangswert
    (op (car sequenz) (akkumuliere op anfangswert (cdr sequenz)))))

(define (abb p sequenz)
  (akkumuliere (lambda (x y) (cons (p x) y)) null sequenz))

(define (append1 seq1 seq2)
  (akkumuliere cons seq2 seq1))

(define (length1 sequenz)
  (akkumuliere (lambda (x y) (+ 1 y)) 0 sequenz))

(define x (list 1 2 3 4 5))

(define y (list 6 7 8 9 10))

(define (kubik x) (* x x x))
