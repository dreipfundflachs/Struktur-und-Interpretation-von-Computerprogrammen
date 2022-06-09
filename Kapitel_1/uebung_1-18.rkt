;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.18 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (verdoppeln n) (+ n n))

(define (halbieren n)
  (if (gerade? n)
    (/ n 2)
    (error "Es ist unmöglich, eine ungerade Zahl zu halbieren!")))

(define (gerade? n) (= (remainder n 2) 0))

(define (schnell-mul m n) (mul-iter m n 0))

(define (mul-iter m n produkt)
  (cond ((= n 0) produkt)
        ((< n 0) (- (mul-iter m (- n) produkt)))
        ((gerade? n) (mul-iter m (halbieren n) (verdoppeln produkt)))
        (else (mul-iter m (- n 1) (+ m produkt)))))
