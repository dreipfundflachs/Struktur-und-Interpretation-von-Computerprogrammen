;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.33 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (filter-akkumulator pred kombinierer null-groesse term a naechstes b)
  (cond ((> a b) null-groesse)
        ((pred a)
         (kombinierer (term a)
                      (filter-akkumulator pred kombinierer null-groesse
                                          term (naechstes a) naechstes b)))
        (else (filter-akkumulator pred kombinierer null-groesse
                                  term (naechstes a) naechstes b))))

(define (kleinster-teiler n)
  (finde-teiler n 2))

(define (finde-teiler n pruef-teiler)
  (cond ((> (quadrat pruef-teiler) n) n)
        ((teilt? pruef-teiler n) pruef-teiler)
        (else (finde-teiler n (+ pruef-teiler 1)))))

(define (teilt? a b)
  (= (remainder b a) 0))

(define (primzahl? n)
  (and (> n 1) (= n (kleinster-teiler n))))

(define (quadrat x) (* x x))

(define plus (lambda (x y) (+ x y)))

(define mal (lambda (x y) (* x y)))

(define (inc x) (+ 1 x))

(define (identitaet x) x)

(define (ggt a b)
  (if (= 0 b)
    a
    (ggt b (remainder a b))))

(define (teilerfremd? n) (lambda (m) (= 1 (ggt m n))))

; a.

(define (primzahl-quadrat-summe a b)
  (filter-akkumulator primzahl? plus 0 quadrat a inc b))

; b.

(define (teilerfremd-produkt n)
  (filter-akkumulator (teilerfremd? n) mal 1 identitaet 1 inc n))
