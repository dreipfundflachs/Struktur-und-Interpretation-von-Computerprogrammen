;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.24 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (runtime) (current-milliseconds))

(define (potmod basis exponent m)
  (cond ((= exponent 0) 1)
        ((gerade? exponent) (remainder
                              (quadrat (potmod basis (/ exponent 2) m))
                              m))
        (else (remainder (* (potmod basis (- exponent 1) m) basis)
                         m))))

(define (fermat-test n)
  (define (versuch a)
  (= (potmod a n n) a))
  (versuch (+ 2 (random (- n 2)))))

(define (schnell-primzahl? n x-mal)
  (cond ((= x-mal 0) true)
        ((fermat-test n) schnell-primzahl? n (- x-mal 1))
        (else false)))

(define (ausgabe-laufzeit laufzeit)
  (display " *** ")
  (display laufzeit))

(define (start-primzahl-test n startzeit)
  (when (schnell-primzahl? n 40)
    (ausgabe-laufzeit (- (runtime) startzeit))))

(define (primzahl-test-zeit n)
  (newline)
  (display n)
  (newline)
  (start-primzahl-test n (runtime)))

(define (primzahl-suche a b anzahl)
  (cond [(or (= anzahl 0) (> a b))
         (newline)
         (display " *** Ende ***")]
        [(gerade? a) (primzahl-suche (+ a 1) b anzahl)]
        [(schnell-primzahl? a 40)
         (newline)
         (display "Found prime: ")
         (primzahl-test-zeit a)
         (primzahl-suche (+ a 1) b (- anzahl 1))]
        [else (primzahl-suche (+ a 1) b anzahl)]))

(define (kleinster-teiler n)
  (finde-teiler n 2))

(define (finde-teiler n pruef-teiler)
  (cond ((> (quadrat pruef-teiler) n) n)
        ((teilt? pruef-teiler n) pruef-teiler)
        (else (finde-teiler n (+ pruef-teiler 1)))))

(define (teilt? a b)
  (= (remainder b a) 0))

(define (primzahl? n)
  (= n (kleinster-teiler n)))

(define (quadrat x) (* x x))

(define (gerade? n) (= 0 (remainder n 2)))

; Beispiel:

; (primzahl-suche 1000000 2000000 3)
