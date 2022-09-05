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
     (display " ist eine Primzahl oder vielleicht eine Carmichael-Zahl.")
     (newline)]
    [begin (display n)
     (display " ist weder eine Primzahl noch eine Carmichael-Zahl.")
     (newline)]))

; Beispiele:

(primzahl? 561) ; ist durch 3 teilbar.

(primzahl? 1105) ; ist durch 5 teilbar.

(primzahl? 1729) ; ist durch 7 teilbar.

(primzahl? 2465) ; ist durch 5 teilbar.

(primzahl? 2821); ist durch 7 teilbar. 

(primzahl? 6601) ; ist durch 7 teilbar.
