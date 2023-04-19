;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.62 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide vereinigungs-menge vereinigung
         nullmenge menge-3 menge-5 menge-gerade menge-ungerade)

(define (vereinigung menge-1 menge-2)
  (cond [(null? menge-1) menge-2]
        [(null? menge-2) menge-1]
        [else (let [(x1 (car menge-1)) (x2 (car menge-2))]
                (cond [(= x1 x2) (cons x1 (vereinigung (cdr menge-1)
                                                       (cdr menge-2)))]
                      [(< x1 x2) (cons x1 (vereinigung (cdr menge-1)
                                                       menge-2))]
                      [(< x2 x1) (cons x2 (vereinigung menge-1
                                                       (cdr menge-2)))]))]))

(define vereinigungs-menge vereinigung)

; Beispiele:

(define nullmenge null)

(define menge-3 (list 0 1 2 3))

(define menge-5 (list 0 1 2 3 4 5))

(define menge-ungerade (list 1 3 5 7 9))

(define menge-gerade (list 0 2 4 6 8 10))
