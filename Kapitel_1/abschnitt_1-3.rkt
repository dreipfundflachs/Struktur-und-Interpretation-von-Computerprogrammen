;;;;;;;;;;;;;;;;;;;;;;;;;;
;  SICP - Abschnitt 1.3  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (kubik x) (* x x x))

(define (summe-ganze-zahlen a b)
  (if (> a b)
    0
    (+ a (summe-ganze-zahlen (+ a 1) b))))

(define (kubik-summe a b)
  (if (> a b)
    0
    (+ (kubik a) (kubik-summe (+ a 1) b))))

(define (pi-summe a b)
  (if (> a b)
    0
    (+ (/ 1.0 (* a (+ a 2))) (pi-summe (+ a 4) b))))

(define (summe term a naechstes b)
  (if (> a b)
    0
    (+ (term a) (summe term (naechstes a) naechstes b))))

(define (inc n) (+ n 1))

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
