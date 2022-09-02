;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.31 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (produkt term a naechstes b)
  (if (> a b)
    1
    (* (term a) (produkt term (naechstes a) naechstes b))))

(define (produkt-iter term a naechstes b)
  (define (iter a ergebnis)
    (if (> a b)
      ergebnis
      (iter (naechstes a) (* ergebnis (term a)))))
  (iter a 1))

(define (inc n) (+ n 1))

(define (identitaet x) x)

(define (fac n) (produkt identitaet 1 inc n))

(define (fac-iter n) (produkt-iter identitaet 1 inc n))

(define (pi-naechstes n) (+ n 2))

(define (pi-term n) (/ (* n n) (* (- n 1) (+ n 1))))

(define (pi-naeherung n)
  (* (/ 8.0 3.0) (produkt-iter pi-term 4 pi-naechstes n)))
