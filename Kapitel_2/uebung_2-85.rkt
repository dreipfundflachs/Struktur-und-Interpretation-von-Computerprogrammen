;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.85 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide erniedrigen projektion projektierbar anwenden-generisch)


(define (installieren-rationales-package)
  ;; interne Prozeduren:
  (define (projektion r)
    (konstr-ganzzahlig (floor (/ (zaehler r) (nenner r)))))
  ; ...
  ;; Schnittstelle zum übrigen System:
  (put 'projektion '(rational) projektion)
  (put 'equ? '(rational rational)
       (lambda (r s) (and (= (nenner r) (nenner s))
                          (= (zaehler r) (zaehler s)))))
  ; ...
  'fertig)

(define (installieren-reelles-package)
  ;; interne Prozeduren:
  (define (projektion t)
    (let ([str-t (~v (+ t 0.0))])
      (let ([d (string-length str-t)])
        (konstr-rational (round (* t (expt 10 d))) (expt 10 d)))))
  ; ...
  ;; Schnittstelle zum übrigen System:
  (put 'erhoehen '(reell) projektion)
  (put 'equ? '(reell reell)
       (lambda (s t) (= s t)))
  ; ...
  'fertig)

(define (installieren-komplex-package)
  ;; interne Prozeduren:
  (define (projektion z) (konstr-reell (reeller-teil z)))
  ;...
  ;; Schnittstelle zum übrigen System:
  (put 'projektion '(komplex) projektion)
  (put 'equ? '(komplex komplex)
       (lambda (z w) (and (= (reeller-teil z) (reeller-teil w))
                          (= (imag-teil z) (imag-teil w)))))
  ; ...
  'fertig)

; Generische Operation 'projektion':
(define (projektion x)
  ((get 'projektion (list (typ-etikett x))) x))

; Generische Operation 'equ?':
(define (equ? x)
  ((get 'equ? (list (typ-etikett x)))))

(define (projektierbar? x)
  (equ? (x (erhoehen (projektion x)))))

; Definition von 'erniedrigen':
(define (erniedrigen x)
  (if (not (projektierbar x))
    x
    (erniedrigen (projektion x))))

; Definition von anwenden-generisch wie vorher
; ...

(define (anwenden-generisch) (compose projektion anwenden-generisch))
