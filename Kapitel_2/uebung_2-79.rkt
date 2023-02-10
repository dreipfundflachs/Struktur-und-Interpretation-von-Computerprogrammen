;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.79 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide equ?)

(define (installieren-scheme-zahl-package)
  ; ...
  (put 'equ? '(scheme-zahl scheme-zahl) =)
  ; ...
  'fertig)

(define (installieren-rationales-package)
  ; ...
  (put 'equ? '(rational rational)
       (lambda (r s)
         (and (= (nenner r) (nenner s))
              (= (zaehler r) (zaehler s)))))
  ; ...
  'fertig)

(define (installieren-komplex-package)
  ; ...
  (put 'equ? '(komplex komplex)
       (lambda (z w)
         (and (= (reeller-teil z) (reeller-teil w))
              (= (imag-teil z) (imag-teil w)))))
  ; ...
  'fertig)

(define (equ? x y)
  (if (eq? (typ-etikket x) (typ-etikett y))
    (anwenden-generisch 'equ? x y)
    #f))
