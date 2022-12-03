;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.54 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide equal1?)

(define (equal1? as bs)
  (cond [(and (not (pair? as)) (not (pair? bs))) (eq? as bs)]
        [(and (pair? as) (pair? bs))
         (and (equal1? (car as) (car bs))
              (equal1? (cdr as) (cdr bs)))]
        [else false]))

; Beispiele:

'(1 2 3 (4 5) (6 7 8))

'(1 2 3 (4 5) (6 7 8))

(equal1?  '(1 2 3 (4 5) (6 7 8)) '(1 2 3 (4 5) (6 7 8)))


'(1 2 3 ((4 5) 6))

'(1 2 3 ((4 (5 6))))

(equal1? '(1 2 3 ((4 5) 6)) '(1 2 3 ((4 (5 6))))) 

