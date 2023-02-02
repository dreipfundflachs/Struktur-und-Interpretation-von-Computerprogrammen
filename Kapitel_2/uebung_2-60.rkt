;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.60 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide vereinigungs-menge schnitt-menge element-der-menge?
         hinzufuegen-menge entferne-element
         nullmenge menge-3 menge-5 menge-gerade menge-ungerade)


(define (hinzufuegen-menge x menge) (cons x menge))
; hinzufuegen-menge ist in O(1)

(define (vereinigungs-menge menge-1 menge-2) (append menge-1 menge-2))
; vereinigungs-menge ist in O(n), worin n die Mächtigkeit von menge-1 ist
; (inklusive die Doppelvorkommen).

(define (element-der-menge? x menge)
  (cond [(null? menge) false]
        [(equal? x (car menge)) true]
        [else (element-der-menge? x (cdr menge))]))
; element-der-menge ist in O(mn), worin m (bzw. n) die Mächtigkeit von menge-1
; (bzw. menge-2) ist (inklusive die Doppelvorkommen).

(define (entferne-element x menge)
  (cond [(null? menge) null]
        [(equal? x (car menge))
         (entferne-element x (cdr menge))]
        [else (cons (car menge) (entferne-element x (cdr menge)))]))
; entferne-element ist in O(n), worin n die Mächtigkeit der Menge ist
; (inklusive die Doppelvorkommen).

(define (schnitt-menge menge-1 menge-2)
  (cond [(or (null? menge-1) (null? menge-2))
         null]
        [(element-der-menge? (car menge-1) menge-2)
         (let [(x (car menge-1))]
           (cons x
                 (schnitt-menge (entferne-element x (cdr menge-1))
                                (entferne-element x menge-2))))]
        [else (schnitt-menge (cdr menge-1) menge-2)]))
; Es seien m (bzw. n) die Mächtigkeit von menge-1 (bzw. menge-2) (inklusive
; die Doppelvorkommen). Im schlimmsten Fall, brauchen wir:
;   * n + (m - 1) + n Operationen im ersten Schritt (denn element-der-menge?
;     erfordert n Operation, und die Anwendung von entferne-element auf (cdr
;     menge-1) (bzw. menge-2) erfordern (m - 1) (bzw. n) Operationen.
;   * (n - 1) + (m - 2) + (n - 1) Operationen im zweiten Schritt.
;   * usw.
; Insgesamt erfordert schnitt-menge also: n(n+1) + m(m-1) / 2 Operationen. Also
; für m = n, ist schnitt-menge in O(n^2) enthalten.


; Beispiele:

(define nullmenge null)

(define menge-3 (list 0 1 2 2 3 3 3))

(define menge-5 (list 0 1 2 2 3 3 3 4 4 4 4 5 5 5 5 5))

(define menge-ungerade (list 1 1 3 3 5 7 9 5 7 9))

(define menge-gerade (list 0 2 4 6 8 10 0 2 4 6 8 10 10 8 6 4 2 0))
