;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.27 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (potmod basis exponent m)
  (cond ((= exponent 0) 1)
        ((gerade? exponent) (remainder
                              (quadrat (potmod basis (/ exponent 2) m))
                              m))
        (else (remainder (* (potmod basis (- exponent 1) m) basis)
                         m))))

(define (quadrat x) (* x x))

(define (gerade? n) (= 0 (remainder n 2)))

(define (fermat-test n)
  (define (test n k)
    (cond ((>= k n) true)
          (else (and
                  (= k (potmod k n n))
                  (test n (+ k 1))))))
  (test n 1))

(define (primzahl? n)
  (if (fermat-test n)
    [begin (display n)
     (display " is a prime number or maybe a Carmichael number.")
     (newline)]
    [begin (display n)
     (display " is certainly _not_ a prime number, nor a Carmichael number.")
     (newline)]))

; Beispiele:

(primzahl? 561) ; ist teilbar durch 3.

(primzahl? 1105) ; ist teilbar durch 5.

(primzahl? 1729) ; ist teilbar durch 7.

(primzahl? 2465) ; ist teilbar durch 5.

(primzahl? 2821); ist teilbar durch 7. 

(primzahl? 6601) ; ist teilbar durch 7.
