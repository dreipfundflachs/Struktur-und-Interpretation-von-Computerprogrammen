;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.45 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide geteilt)

(define (geteilt op1 op2)
  (lambda (maler n)
    (if (= n 0)
      maler
      (let ((kleiner ((geteilt op1 op2) maler (- n 1))))
        (op1 maler (op2 kleiner kleiner))))))
