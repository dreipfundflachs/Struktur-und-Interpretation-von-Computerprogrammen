;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.16 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (schnell-pot b n)
  (pot-iter b n 1)) 

(define (pot-iter b zaehler a)
  (cond ((= zaehler 0) a)
        ((gerade? zaehler) (pot-iter (quadrat b) (/ zaehler 2) a))
        (else (pot-iter b (- zaehler 1) (* b a)))))

(define (gerade? n) (= (remainder n 2) 0))

(define (quadrat x) (* x x))
