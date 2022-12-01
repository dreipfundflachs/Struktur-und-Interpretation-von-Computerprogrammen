;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.44 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide oben-geteilt)

(define (oben-geteilt maler n)
  (if (= n 0)
    maler
    (let ((kleiner (oben-geteilt maler (- n 1))))
      (unter maler (neben kleiner kleiner)))))
