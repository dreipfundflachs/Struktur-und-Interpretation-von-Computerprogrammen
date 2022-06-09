;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.17 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mul a b)
  (if (= b 0)
    0
    (+ a (mul a (- b 1)))))

(define (verdoppeln n) (+ n n))

(define (halbieren n)
  (if (gerade? n)
    (/ n 2)
    (error "Es ist unmöglich, eine ungerade Zahl zu halbieren!")))

(define (gerade? n) (= (remainder n 2) 0))

(define (schnell-mul m n)
  (cond ((= n 0) 0)
        ((< n 0) (- (schnell-mul m (- n))))
        ((gerade? n) (verdoppeln (schnell-mul m (halbieren n))))
        (else (+ m (schnell-mul m (- n 1))))))
