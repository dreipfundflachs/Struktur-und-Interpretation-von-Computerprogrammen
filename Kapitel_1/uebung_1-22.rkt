;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.21 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (runtime) (current-milliseconds))

(define (ausgabe-laufzeit laufzeit)
  (display " *** ")
  (display laufzeit))

(define (start-primzahl-test n startzeit)
  (when (primzahl? n)
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
        [(primzahl? a)
         (newline)
         (display "Found prime: ")
         (display a)
         (primzahl-suche (+ a 2) b (- anzahl 1))]
        [else (primzahl-suche (+ a 2) b anzahl)]))

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
