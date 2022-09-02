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

(define (pi-summe a b)
  (summe (lambda (x) (/ 1.0 (* x (+ x 2))))
         a
         (lambda (x) (+ x 4))
         b))

(define (integral f a b dx)
  (* (summe f
            (+ a (/ dx 2.0))
            (lambda (x) (+ x dx))
            b)
     dx))

(define (f x y)
  (define (hilfs-f a b)
    (+ (* x (quadrat a)) (* y b) (* a b)))
  (hilfs-f (+ 1 (* x y)) (- 1 y)))

(define (f x y)
  ((lambda (a b)
     (+ (* x (quadrat a))
        (* y b)
        (* a b)))
   (+ 1 (* x y)) (- 1 y)))

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (quadrat a)) (* y b) (* a b))))

(define (kubik x) (* x x x))

(define (suche f neg-punkt pos-punkt eps)
  (let ((mittel-punkt (mittelwert neg-punkt pos-punkt)))
        (if (nah-genug? neg-punkt pos-punkt eps)
          mittel-punkt
          (let ((test-wert (f mittel-punkt)))
            (cond ((positive? test-wert)
                   (suche f neg-punkt mittel-punkt))
                  ((negative? test-wert)
                   (suche f mittel-punkt pos-punkt))
                  (else mittel-punkt))))))

(define (nah-genug? x y eps)
  (< (abs (- x y)) eps))

(define (mittelwert a b)
  (/ (+ a b) 2))
