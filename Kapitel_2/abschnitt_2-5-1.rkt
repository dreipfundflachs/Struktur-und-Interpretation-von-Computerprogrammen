;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  SICP - Abschnitt 2.5.1  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide add sub mul div
         etikettieren typ-etikett inhalt
         installieren-scheme-zahl-package
         installieren-rationales-package
         installieren-komplex-package
         konstr-scheme-zahl konstr-rational
         )

(define (add x y) (anwenden-generisch 'add x y))
(define (sub x y) (anwenden-generisch 'sub x y))
(define (mul x y) (anwenden-generisch 'mul x y))
(define (div x y) (anwenden-generisch 'div x y))

(define (etikettieren etik obj) (cons etik obj))

(define (typ-etikett datum)
  (if (pair? datum)
    (car datum)
    (error "Fehler beim Datentyp -- TYP-ETIKETT" datum)))

(define (inhalt datum)
  (if (pair? datum)
    (cdr datum)
    (error "Fehler beim Datentyp -- INHALT" datum)))

(define (installieren-scheme-zahl-package)
  (define (etikett x)
    (etikettieren 'scheme-zahl x))
  (put 'add '(scheme-zahl scheme-zahl)
       (lambda (x y) (etikett (+ x y))))
  (put 'sub '(scheme-zahl scheme-zahl)
       (lambda (x y) (etikett (- x y))))
  (put 'mul '(scheme-zahl scheme-zahl)
       (lambda (x y) (etikett (* x y))))
  (put 'div '(scheme-zahl scheme-zahl)
       (lambda (x y) (etikett (/ x y))))
  (put 'konstr 'scheme-zahl
       (lambda (x) (etikett x)))
  'fertig)

(define (konstr-scheme-zahl n)
  ((get 'konstr 'scheme-zahl) n))

(define (installieren-rationales-package)
  ;; interne Prozeduren:
  (define (zaehler z) (car z))
  (define (nenner z) (cdr z))
  (define (ggt a b)
    (if (= b 0)
      a
      (ggt b (modulo a b))))
  (define (konstr-rat n d)
    (let ((g (ggt n d)))
    (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (konstr-rat (+ (* (zaehler x) (nenner y))
                   (* (zaehler y) (nenner x)))
                (* (nenner x) (nenner y))))
  (define (sub-rat x y)
    (konstr-rat (- (* (zaehler x) (nenner y))
                   (* (zaehler y) (nenner x)))
                (* (nenner x) (nenner y))))
  (define (mul-rat x y)
    (konstr-rat (* (zaehler x) (zaehler y))
                (* (nenner x) (nenner y))))
  (define (div-rat x y)
    (konstr-rat (* (zaehler x) (nenner y))
                (* (nenner x) (zaehler y))))
  (define (gleich-rat? x y)
    (= (* (zaehler x) (nenner y))
       (* (zaehler y) (nenner x))))

  ;; Schnittstelle zum übrigen System:
  (define (etikett x) (etikettieren 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (etikett (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (etikett (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (etikett (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (etikett (div-rat x y))))

  (put 'konstr 'rational
       (lambda (n d) (etikett (konstr-rat n d))))
  'fertig)

(define (konstr-rational n d)
  ((get 'konstr 'rational) n d))

(define (installieren-komplex-package)
  ;; importierte Prozeduren aus den Rechteck- und Polar-Packages:
  (define (konstr-aus-reell-imag x y)
    ((get 'konstr-aus-reell-imag 'rechteck) x y))
  (define (konstr-aus-abs-wkl r a)
    ((get 'konstr-aus-abs-wkl 'polar) r a))

  ;; interne Prozeduren:
  (define (add-komplex z1 z2)
    (konstr-aus-reell-imag (+ (reeller-teil z1)
                              (reeller-teil z2))
                           (+ (imag-teil z1)
                              (imag-teil z2))))
  (define (sub-komplex z1 z2)
    (konstr-aus-reell-imag (- (reeller-teil z1)
                              (reeller-teil z2))
                           (- (imag-teil z1)
                              (imag-teil z2))))
  (define (mul-komplex z1 z2)
    (konstr-aus-abs-wkl (* (abs-wert z1)
                           (abs-wert z2))
                        (+ (winkel z1)
                           (winkel z2))))
  (define (div-komplex z1 z2)
    (konstr-aus-abs-wkl (/ (abs-wert z1)
                           (abs-wert z2))
                        (- (winkel z1)
                           (winkel z2))))

  ;; Schnittstelle zum übrigen System:
  (define (etikett z) (etikettieren 'komplex x))
  (put 'add '(komplex komplex)
       (lambda (x y) (etikett (add-komplex x y))))
  (put 'sub '(komplex komplex)
       (lambda (x y) (etikett (sub-komplex x y))))
  (put 'mul '(komplex komplex)
       (lambda (x y) (etikett (mul-komplex x y))))
  (put 'div '(komplex komplex)
       (lambda (x y) (etikett (div-komplex x y))))
  (put 'konstr-aus-reell-imag 'komplex
       (lambda (x y) (etikett (konstr-aus-reell-imag x y))))
  (put 'konstr-aus-abs-wkl 'komplex
       (lambda (r a) (etikett (konstr-aus-abs-wkl r a))))
;   (put 'reeller-teil '(komplex) reeller-teil)
;   (put 'imag-teil '(komplex) imag-teil)
;   (put 'abs-wert '(komplex) abs-wert)
;   (put 'winkel '(komplex) winkel)
  'fertig)

(define (konstr-komplex-aus-reell-imag x y)
  ((get 'konstr-aus-reell-imag 'komplex) x y))

(define (konstr-komplex-aus-abs-wkl r a)
  ((get 'konstr-aus-abs-wkl 'komplex) r a))
