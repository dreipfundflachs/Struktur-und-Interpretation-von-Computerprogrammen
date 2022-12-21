;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.37 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide akkumuliere akkumuliere-n
         skalar-produkt matrix-*-vektor transponiere matrix-*-matrix
         m n v w)


; Akkumulieren:

(define (akkumuliere op anfangswert sequenz)
  (if (null? sequenz)
    anfangswert
    (op (car sequenz) (akkumuliere op anfangswert (cdr sequenz)))))

(define (akkumuliere-n op anfangswert sequenzen)
  (if (null? (car sequenzen))
    null
    (cons (akkumuliere op anfangswert (map car sequenzen))
          (akkumuliere-n op anfangswert (map cdr sequenzen)))))


; Operationen:

(define (summe liste)
  (if (null? liste)
    0
    (+ (car liste) (summe (cdr liste)))))

(define (skalar-produkt v w)
  (summe (akkumuliere-n * 1 (list v w))))

(define (matrix-*-vektor m v)
  (if (null? m)
    null
    (cons (skalar-produkt (car m) v) (matrix-*-vektor (cdr m) v))))

(define (transponiere m)
  (akkumuliere-n cons null m))

(define (matrix-*-matrix m n)
  (let ((spalten (transponiere n)))
    (map (lambda (v) (matrix-*-vektor spalten v)) m)))


; Beispiele:

(define m (list (list 1 2 3 4)
                (list 4 5 6 6)
                (list 6 7 8 9)))

(define n (list (list 0 1 2 3)
                (list 4 5 6 7)
                (list 8 9 10 11)
                (list 11 12 13 14)))

(define v (list 0 1 2 3))

(define w (list 4 5 6 7))
