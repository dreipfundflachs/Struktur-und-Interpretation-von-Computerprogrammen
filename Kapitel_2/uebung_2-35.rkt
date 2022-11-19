;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.35 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide akkumuliere zaehle-blaetter b)

(define (akkumuliere op anfangswert sequenz)
  (if (null? sequenz)
    anfangswert
    (op (car sequenz) (akkumuliere op anfangswert (cdr sequenz)))))

(define (zaehle-blaetter baum)
  (akkumuliere +
               0
               (map (lambda (ast)
                      (cond ((not (pair? ast)) 1)
                            (else (zaehle-blaetter ast))))
                    baum)))

(define b (list 1
                (list 2 (list 3 4) 5)
                (list 6 7)))
