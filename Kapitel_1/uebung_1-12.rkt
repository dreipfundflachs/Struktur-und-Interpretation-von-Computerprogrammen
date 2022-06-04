;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.12 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pascal n k)
  (cond ((or (= k n) (= k 0)) 1)
        (else (+ (pascal (- n 1) (- k 1))
                 (pascal (- n 1) k)))))
