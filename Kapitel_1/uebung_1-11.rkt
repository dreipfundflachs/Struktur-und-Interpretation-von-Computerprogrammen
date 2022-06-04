;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.11 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1))
                 (f (- n 2))
                 (f (- n 3))
                 ))))

(define (trib n) 
    (define (trib-iter a b c zaehler)
      (cond ((= zaehler 0) c)
            (else (trib-iter (+ a b c) a b (- zaehler 1)))))
(trib-iter 2 1 0 n)
)
