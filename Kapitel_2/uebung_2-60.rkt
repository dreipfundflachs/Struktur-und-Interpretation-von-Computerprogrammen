;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.60 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide vereinigungs-menge schnitt-menge element-der-menge?
         hinzufuegen-menge entferne-element
         nullmenge menge-3 menge-5 menge-gerade menge-ungerade)

(define (element-der-menge? x menge)
  (cond [(null? menge) false]
        [(equal? x (car menge)) true]
        [else (element-der-menge? x (cdr menge))]))

(define (hinzufuegen-menge x menge) (cons x menge))

(define (vereinigungs-menge menge-1 menge-2) (append menge-1 menge-2))

(define (entferne-element x menge)
  (cond [(null? menge) null]
        [(equal? x (car menge))
         (entferne-element x (cdr menge))]
        [else (cons (car menge) (entferne-element x (cdr menge)))]))

(define (schnitt-menge menge-1 menge-2)
  (cond [(or (null? menge-1) (null? menge-2))
         null]
        [(element-der-menge? (car menge-1) menge-2)
         (let [(x (car menge-1))]
           (cons x
                 (schnitt-menge (entferne-element x (cdr menge-1))
                                (entferne-element x (cdr menge-2)))))]
        [else (schnitt-menge (cdr menge-1) menge-2)]))


; Beispiele:

(define nullmenge null)

(define menge-3 (list 0 1 2 2 3 3 3))

(define menge-5 (list 0 1 2 2 3 3 3 4 4 4 4 5 5 5 5 5))

(define menge-ungerade (list 1 1 3 3 5 7 9 5 7 9))

(define menge-gerade (list 0 2 4 6 8 10 0 2 4 6 8 10 10 8 6 4 2 0))
