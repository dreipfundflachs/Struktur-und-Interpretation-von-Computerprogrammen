;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.29 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (summe term a naechstes b)
  (if (> a b)
    0
    (+ (term a) (summe term (naechstes a) naechstes b))))

(define (simpson-term f)
  (lambda (x) (f x)))

(define (simpson-naechstes dx)
  (lambda (a) (+ a dx)))

; Man bemerke, dass n eine _gerade_ ganze Zahl sein muss!
(define (simpson-regel f a b n)
  (let ((h (/ (- b a) n)))
      (* (/ h 3.0) (+ (* 2 (summe (simpson-term f) a
                           (simpson-naechstes (* 2 h)) b))
                      (* 4 (summe (simpson-term f) (+ a h)
                           (simpson-naechstes (* 2 h)) b))
                      (- (+ (f a) (f b)))))))


(define (kubik x) (* x x x))

; Integral von 0 bis 1 über g = 1 / 7
(define (g x) (* (kubik x) (kubik x)))

(display " 1/7 = ")
(display (/ 1 7.0))

(newline)
