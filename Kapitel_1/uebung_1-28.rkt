;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.28 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (quadrat x) (* x x))

(define (gerade? n) (= 0 (remainder n 2)))

(define (quadrat-pruefen a m)
  (if (and (not (= a 1))
           (not (= a (- m 1)))
           (= 1 (remainder (quadrat a) m)))
    0
    (remainder (quadrat a) m)))

(define (potmod-signal basis exponent m)
  (cond ((= exponent 0) 1)
        ((gerade? exponent)
         (potmod-signal (quadrat-pruefen basis m) (/ exponent 2) m))
        (else (remainder (* basis (potmod-signal basis (- exponent 1) m)) m))))

(define (miller-rabin p anzahl)
  (cond ((= anzahl 0)
         true)
        ((not (= (potmod-signal (+ 1 (random (- p 1))) (- p 1) p) 1))
         false)
        (else (miller-rabin p (- anzahl 1)))))
