;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.40 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide durchzaehlen-paare durchzaehlen glaetten eindeutige-paare
         primzahl-summe-paare primzahl-summe?)

(define (durchzaehlen i j)
  (if (> i j)
    null
    (cons i (durchzaehlen (+ i 1) j))))

(define (durchzaehlen-paare x i j)
  (if (> i j)
    null
    (cons (list x i) (durchzaehlen-paare x (+ i 1) j))))

(define (bilde-paare n)
  (map (lambda (i) (durchzaehlen-paare i (+ i 1) n))
       (durchzaehlen 1 n)))

(define (glaetten xss) (foldr append null xss))

(define (eindeutige-paare n)
  (glaetten (bilde-paare n)))

(define (primzahl-summe? paar)
  (primzahl? (+ (car paar) (cadr paar))))

(define (primzahl-summe-paare n)
  (filter primzahl-summe? (eindeutige-paare n)))

; Prozeduren zur Berechnung von Primzahlen:

(define (kleinster-teiler n)
  (finde-teiler n 2))

(define (finde-teiler n pruef-teiler)
  (cond ((> (quadrat pruef-teiler) n) n)
        ((teilt? pruef-teiler n) pruef-teiler)
        (else (finde-teiler n (+ pruef-teiler 1)))))

(define (teilt? a b)
  (= (remainder b a) 0))

(define (primzahl? n)
  (= n (kleinster-teiler n)))

(define (quadrat x) (* x x))

; Beispiel:

(eindeutige-paare 6)

(primzahl-summe-paare 6)
