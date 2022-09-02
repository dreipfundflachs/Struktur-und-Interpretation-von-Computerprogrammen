;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.23 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (kleinster-teiler n) (finde-teiler n 2))

(define (finde-teiler n pruef-teiler)
  (define (naechstes k)
    (if (= k 2)
      3
      (+ k 2)))
  (cond ((> (quadrat pruef-teiler) n) n)
        ((teilt? pruef-teiler n) pruef-teiler)
        (else (finde-teiler n (naechstes pruef-teiler)))))

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
         (display "Primzahl gefunden: ")
         (primzahl-test-zeit a)
         (primzahl-suche (+ a 2) b (- anzahl 1))]
        [else (primzahl-suche (+ a 2) b anzahl)]))

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

; Beispiele:

; (primzahl-suche 1000000000 2000000000 3)
; (primzahl-suche 10000000000 20000000000 3)
; (primzahl-suche 100000000000 200000000000 3)
; (primzahl-suche 1000000000000 2000000000000 3)
; (primzahl-suche 10000000000000 20000000000000 3)
