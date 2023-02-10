;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.80 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide equ?)

(define (installieren-scheme-zahl-package)
  ; ...
  (put '=null? '(scheme-zahl scheme-zahl)
       (lambda (x) (and (= 0 x))))
  ; ...
  'fertig)

(define (installieren-rationales-package)
  ; ...
  (put '=null? '(rational rational)
       (lambda (r) (= 0 (zaehler r))))
  ; ...
  'fertig)

(define (installieren-komplex-package)
  ; ...
  (put '=null? '(komplex komplex)
       (lambda (z)
         (and (= 0 (reeller-teil z))
              (= 0 (imag-teil z)))))
  ; ...
  'fertig)

(define (=null? x) (anwenden-generisch '=null? x))
