;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  SICP - Abschnitt 2.3.1  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide memq_1)

(define (memq_1 element xs)
  (cond ((null? xs) false)
        ((eq? element (car xs)) (cdr xs))
        (else (memq_1 element (cdr xs)))))
