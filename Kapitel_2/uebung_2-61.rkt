;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.61 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide hinzufuegen-menge
         nullmenge menge-3 menge-5 menge-gerade menge-ungerade)

(define (hinzufuegen-menge x menge)
  (if (null? menge)
    (list x)
    (let [(y (car menge))]
      (cond [(= x y) menge]
            [(< x y) (cons x menge)]
            [else (cons y (hinzufuegen-menge x (cdr menge)))]))))

; Beispiele:

(define nullmenge null)

(define menge-3 (list 0 1 2 3))

(define menge-5 (list 0 1 2 3 4 5))

(define menge-ungerade (list 1 3 5 7 9))

(define menge-gerade (list 0 2 4 6 8 10))
