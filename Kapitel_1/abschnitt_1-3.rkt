;;;;;;;;;;;;;;;;;;;;;;;;;;
;  SICP - Abschnitt 1.3  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (quadrat x) (* x x))

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


(define (suche f neg-punkt pos-punkt eps)
  (let ((mittel-punkt (mittelwert neg-punkt pos-punkt)))
        (if (nah-genug? neg-punkt pos-punkt eps)
          mittel-punkt
          (let ((test-wert (f mittel-punkt)))
            (cond ((positive? test-wert)
                   (suche f neg-punkt mittel-punkt eps))
                  ((negative? test-wert)
                   (suche f mittel-punkt pos-punkt eps))
                  (else mittel-punkt))))))

(define (intervall-halbierung f a b eps)
  (let ((a-wert (f a))
        (b-wert (f b)))
    (cond ((and (negative? a-wert) (positive? b-wert))
           (suche f a b eps))
          ((and (positive? a-wert) (negative? b-wert))
           (suche f b a eps))
          (else (error "Werte haben gleiches Vorzeichen" a b)))))

(define (nah-genug? x y eps)
  (< (abs (- x y)) eps))

(define (mittelwert a b)
  (/ (+ a b) 2))

(define (kubik x) (* x x x))

(define (p x) (- (kubik x) (* 2 x) 3))

(define (fixpunkt f schaetzwert toleranz)
  (define (versuch schaetzung)
    (let ((naechstes (mittelwert (f schaetzung) schaetzung)))
      (if (nah-genug? schaetzung naechstes toleranz)
        naechstes
        (begin
          (display naechstes)
          (newline)
          (versuch naechstes)))))
  (versuch schaetzwert))

(define (wurzel x)
  (fixpunkt (lambda (y) (mittelwert y (/ x y))) 1.0 0.00001))
  
(define (mittelwert-daempfung f)
  (lambda (x) (mittelwert x (f x))))

(define epsilon 0.00001)

(define (wurzel x)
  (fixpunkt (mittelwert-daempfung (lambda (y) (/ x y)))
                                  1.0
                                  epsilon))

(define (kubikwurzel x)
  (fixpunkt (mittelwert-daempfung
              (lambda (y) (/ x (quadrat y))))
              1.0
              epsilon))

(define (ableitung g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((ableitung g) x)))))

(define (newton g schaetzung toleranz)
  (fixpunkt (newton-transform g) schaetzung toleranz))

(define (wurzel x)
  (newton (lambda (y) (- (quadrat y) x))
          1.0
          epsilon))

(define (fixpunkt-von-transform g transform schaetzung toleranz)
  (fixpunkt (transform g) schaetzung toleranz))

(define (wurzel x)
  (fixpunkt-von-transform (lambda (y) (/ x y))
                          mittelwert-daempfung
                          1.0
                          epsilon))
