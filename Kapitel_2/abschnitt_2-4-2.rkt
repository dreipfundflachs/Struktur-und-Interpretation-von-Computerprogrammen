;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  SICP - Abschnitt 2.4.2  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide etikettieren typ-etikett inhalt
         rechteck? polar?
         add-komplex sub-komplex mul-komplex div-komplex
         reeller-teil reeller-teil-rechteck reeller-teil-polar
         imag-teil imag-teil-rechteck imag-teil-polar
         abs-wert abs-wert-rechteck abs-wert-polar
         winkel winkel-rechteck winkel-polar
         konstr-aus-reell-imag
         konstr-aus-reell-imag-rechteck
         konstr-aus-reell-imag-polar
         konstr-aus-abs-wkl
         konstr-aus-abs-wkl-rechteck
         konstr-aus-abs-wkl-polar)

; Konstruktoren und Selektoren für Typ-Etiketten
(define (etikettieren typ-etikett inhalt)
  (cons typ-etikett inhalt))

(define (typ-etikett datum)
  (if (pair? datum)
    (car datum)
    (error "Fehler beim Datentyp -- TYP-ETIKETT" datum)))

(define (inhalt datum)
  (if (pair? datum)
    (cdr datum)
    (error "Fehler beim Datentyp -- INHALT" datum)))

(define (rechteck? z)
  (eq? (typ-etikett z) 'rechteck))

(define (polar? z)
  (eq? (typ-etikett z) 'polar))

; Bens Darstellung komplexer Zahlen:
(define (reeller-teil-rechteck z) (car z))

(define (imag-teil-rechteck z) (cdr z))

(define (konstr-aus-reell-imag-rechteck x y)
  (etikettieren 'rechteck (cons x y)))

(define (abs-wert-rechteck z)
  (sqrt (+ (quadrat (reeller-teil z)) (quadrat imag-teil z))))

(define (winkel-rechteck z)
  (atan (imag-teil z) (reeller-teil z)))

(define (konstr-aus-abs-wkl-rechteck r a)
  (etikettieren 'rechteck
                (cons (* r (cos a))
                      (* r (sin a)))))

; Alyssas Darstellung komplexer Zahlen:
(define (abs-wert-polar z) (car z))

(define (winkel-polar z) (cdr z))

(define (konstr-aus-abs-wkl-polar r a)
  (etikettieren 'polar (cons r a)))

(define (reeller-teil-polar z)
  (* (abs-wert z) (cos (winkel z))))

(define (imag-teil-polar z)
  (* (abs-wert z) (sin (winkel z))))

(define (konstr-aus-reell-imag-polar x y)
  (etikettieren 'polar
                (cons (sqrt (+ (quadrat x) (quadrat y)))
                      (atan y x))))

; Generische Selektoren:
(define (reeller-teil z)
  (cond ([rechteck? z]
         (reeller-teil-rechteck (inhalt z)))
        ([polar? z]
         (reeller-teil-polar (inhalt z)))
        (else (error
                "Unbekannter Datentyp -- REELLER-TEIL" z))))

(define (imag-teil z)
  (cond ([rechteck? z]
         (imag-teil-rechteck (inhalt z)))
        ([polar? z]
         (imag-teil-polar (inhalt z)))
        (else (error
                "Unbekannter Datentyp -- IMAG-TEIL" z))))

(define (abs-wert z)
  (cond ([rechteck? z]
         (abs-wert-rechteck (inhalt z)))
        ([polar? z]
         (abs-wert-polar (inhalt z)))
        (else (error
                "Unbekannter Datentyp -- ABS-WERT" z))))

(define (winkel z)
  (cond ([rechteck? z]
         (winkel-rechteck (inhalt z)))
        ([polar? z]
         (winkel-polar (inhalt z)))
        (else (error
                "Unbekannter Datentyp -- WINKEL" z))))

; Implementierung von arithmetischen Operationen:
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

; Implementierung der Konstruktoren:
(define (konstr-aus-reell-imag x y)
  (konstr-aus-reell-imag-rechteck x y))

(define (konstr-aus-abs-wkl r a)
  (konstr-aus-abs-wkl-polar r a))

; Zusätzliche Prozedur:
(define (quadrat t) (* t t))
