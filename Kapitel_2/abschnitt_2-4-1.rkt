;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  SICP - Abschnitt 2.4.1  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide add-komplex sub-komplex mul-komplex div-komplex
         reeller-teil imag-teil abs-wert winkel
         kosntr-aus-reell-imag konstr-aus-abs-wkl)

; Implementation der arithmetischen Operationen:
(define (add-komplex z1 z2)
  (konstr-aus-reell-imag (+ (reeller-teil z1) (reeller-teil z2))
                        (+ (imag-teil z1) (imag-teil z2))))

(define (sub-komplex z1 z2)
  (konstr-aus-reell-imag (- (reeller-teil z1) (reeller-teil z2))
                        (- (imag-teil z1) (imag-teil z2))))

(define (mul-komplex z1 z2)
  (konstr-aus-abs-wkl (* (abs-wert z1) (abs-wert z2))
                      (+ (winkel z1) (winkel z2))))

(define (div-komplex z1 z2)
  (konstr-aus-abs-wkl (/ (abs-wert z1) (abs-wert z2))
                      (- (winkel z1) (winkel z2))))

; Bens Darstellung komplexer Zahlen:
(define (reeller-teil z) (car z))

(define (imag-teil z) (cdr z))

(define (konstr-aus-reell-imag x y) (cons x y))

(define (abs-wert z)
  (sqrt (+ (quadrat (reeller-teil z)) (quadrat imag-teil z))))

(define (winkel z)
  (atan (imag-teil z) (reeller-teil z)))

(define (konstr-aus-abs-wkl r a)
  (konstr-aus-reell-imag (* r (cos a)) (* r (sin a))))

; Alyssas Darstellung komplexer Zahlen:
(define (abs-wert z) (car z))

(define (winkel z) (cdr z))

(define (konstr-aus-abs-wkl r a) (cons r a))

(define (reeller-teil z)
  (* (abs-wert z) (cos (winkel z))))

(define (imag-teil z)
  (* (abs-wert z) (sin (winkel z))))

(define (konstr-aus-reell-imag x y)
  (konstr-aus-abs-wkl
    (sqrt (+ (quadrat x) (quadrat y)))
    (atan y x)))

; Zusätzliche Prozedur:
(define (quadrat t) (* t t))
