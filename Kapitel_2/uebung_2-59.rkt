;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.59 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide vereinigungs-menge element-der-menge?
         nullmenge menge-3 menge-5 menge-gerade menge-ungerade)

(define (element-der-menge? x menge)
  (cond [(null? menge) false]
        [(equal? x (car menge)) true]
        [else (element-der-menge? x (cdr menge))]))

(define (vereinigungs-menge menge-1 menge-2)
  (cond [(null? menge-1) menge-2]
        [(element-der-menge? (car menge-1) menge-2)
         (vereinigungs-menge (cdr menge-1) menge-2)]
        [else (cons (car menge-1)
                    (vereinigungs-menge (cdr menge-1) menge-2))]))

; Beispiele:

(define nullmenge null)

(define menge-3 (list 0 1 2 3))

(define menge-5 (list 0 1 2 3 4 5))

(define menge-ungerade (list 1 3 5 7 9))

(define menge-gerade (list 0 2 4 6 8 10))
