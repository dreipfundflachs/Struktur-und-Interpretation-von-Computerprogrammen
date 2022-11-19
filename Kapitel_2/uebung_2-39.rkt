;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.39 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide fold-left fold-right reverse-l reverse-r liste)

(define (fold-left op ergebnis sequenz)
  (if (null? sequenz)
    ergebnis
    (fold-left op (op ergebnis (car sequenz)) (cdr sequenz))))

(define (fold-right op anfangswert sequenz)
  (if (null? sequenz)
    anfangswert
    (op (car sequenz) (fold-right op anfangswert (cdr sequenz)))))

(define (reverse-r sequenz)
  (fold-right (lambda (x ys) (append ys (list x)))
              null
              sequenz))

(define (reverse-l sequenz)
  (fold-left (lambda (xs y) (cons y xs))
             null
             sequenz))

(define liste (list 1 2 3 4 5))
