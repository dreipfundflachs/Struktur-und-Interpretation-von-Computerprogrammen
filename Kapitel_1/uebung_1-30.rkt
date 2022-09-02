;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.30 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (summe term a naechstes b)
  (define (iter a ergebnis)
    (if (> a b)
      ergebnis
      (iter (naechstes a) (+ ergebnis (term a)))))
    (iter a 0))

; Beispiele:

(define (inc n) (+ n 1))

(define (kubik x) (* x x x))

(define (kubik-summe a b)
  (summe kubik a inc b))

(define (identitaet x) x)

(define (summe-ganze-zahlen a b)
  (summe identitaet a inc b))

(define (pi-summe a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-naechstes x)
    (+ x 4))
  (summe pi-term a pi-naechstes b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (summe f (+ a (/ dx 2.0)) add-dx b)
     dx))
