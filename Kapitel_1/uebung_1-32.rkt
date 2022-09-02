;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.32 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (akkumulator kombinierer null-groesse term a naechstes b)
  (if (> a b)
    null-groesse
    (kombinierer (term a)
                 (akkumulator kombinierer null-groesse
                              term (naechstes a) naechstes b))))

(define plus (lambda (x y) (+ x y)))

(define mal (lambda (x y) (* x y)))

(define (summe term a naechstes b)
  (akkumulator plus 0 term a naechstes b))

(define (produkt term a naechstes b)
  (akkumulator mal 1 term a naechstes b))

(define (akkumulator-iter kombinierer null-groesse term a naechstes b)
  (define (iter a ergebnis)
    (if (> a b)
      ergebnis
      (iter (naechstes a) (kombinierer (term a) ergebnis))))
  (iter a null-groesse))

(define (summe-iter term a naechstes b)
  (akkumulator-iter plus 0 term a naechstes b))

(define (produkt-iter term a naechstes b)
  (akkumulator-iter mal 1 term a naechstes b))

; Beispiele:

(define (inc n) (+ n 1))

(define (identitaet x) x)

(define (kubik x) (* x x x))

(define (kubik-summe a b) (summe kubik a inc b))

(define (fac n) (produkt identitaet 1 inc n))

(define (fac-iter n) (produkt-iter identitaet 1 inc n))

(define (pi-naechstes n) (+ n 2))

(define (pi-term n) (/ (* n n) (* (- n 1) (+ n 1))))

(define (pi-naeherung n)
  (* (/ 8.0 3.0) (produkt-iter pi-term 4 pi-naechstes n)))
