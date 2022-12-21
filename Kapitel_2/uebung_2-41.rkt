;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.41 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide durchzaehlen-paare durchzaehlen glaetten bilde-paare bilde-tripel
         durchzaehlen-tripel kleiner-als-summe-tripel summe-tripel
         summe-kleiner-als?)

(define (durchzaehlen i j)
  (if (> i j)
    null
    (cons i (durchzaehlen (+ i 1) j))))

(define (durchzaehlen-paare x i j)
  (if (> i j)
    null
    (cons (list x i) (durchzaehlen-paare x (+ i 1) j))))

(define (bilde-paare m n)
  (glaetten (map (lambda (i) (durchzaehlen-paare i (+ i 1) n))
       (durchzaehlen m n))))

(define (bilde-tripel i n)
  (map (lambda (paar) (cons i paar)) (bilde-paare (+ i 1) n)))

(define (glaetten xss) (foldr append null xss))

(define (durchzaehlen-tripel n)
  (glaetten (map (lambda (i) (bilde-tripel i n))
                 (durchzaehlen 1 (- n 2)))))

(define (kleiner-als-summe-tripel s n)
  (filter (summe-kleiner-als? s) (durchzaehlen-tripel n)))

(define (summe-kleiner-als? s)
  (lambda (tripel) (< (summe-tripel tripel) s)))

(define (summe-tripel tripel)
  (+ (car tripel) (cadr tripel) (caddr tripel)))

; Beispiel:

(newline)
(display "Alle Triaden (i, j, k) mit 1 <= i < j < k <= 6:")
(newline)
(durchzaehlen-tripel 6)


(newline)
(display "Alle solche Triaden, deren Summe kleiner als 10 ist:")
(newline)
(kleiner-als-summe-tripel 10 6)
