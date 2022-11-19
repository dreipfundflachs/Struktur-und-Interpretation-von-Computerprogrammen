;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.36 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide akkumuliere akkumuliere-n seqn)

(define (akkumuliere op anfangswert sequenz)
  (if (null? sequenz)
    anfangswert
    (op (car sequenz) (akkumuliere op anfangswert (cdr sequenz)))))

(define (akkumuliere-n op anfangswert sequenzen)
  (if (null? (car sequenzen))
    null
    (cons (akkumuliere op anfangswert (map car sequenzen))
          (akkumuliere-n op anfangswert (map cdr sequenzen)))))

(define seqn (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

; Tests:

(equal? (akkumuliere-n + 0 seqn) (list 12 15 18))

(equal? (akkumuliere-n cons null seqn)
        (list (list 1 4 7)
              (list 2 5 8)
              (list 3 6 9)))
