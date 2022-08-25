;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.01 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ggt a b)
  (if (= b 0)
    a
    (ggt b (modulo a b))))

(define (konstr-rat n d)
  (let ((g (ggt (abs n) (abs d))))
    (cond ((and (> n 0) (> d 0))
           (cons (/ n g) (/ d g)))
          ((and (< n 0) (< d 0))
           (cons (/ (abs n) g) (/ (abs d) g)))
          (else
            (cons (- (/ (abs n) g)) (/ (abs d) g))))))

; Andere einschlägige Prozeduren:
(define (zaehler z) (car z))

(define (nenner z) (cdr z))

(define (add-rat x y)
  (konstr-rat (+ (* (zaehler x) (nenner y))
                 (* (nenner x) (zaehler y)))
              (* (nenner x) (nenner y))))

(define (sub-rat x y)
  (konstr-rat (- (* (zaehler x) (nenner y))
                 (* (zaehler y) (nenner y)))
              (* (nenner x) (nenner y))))

(define (mul-rat x y)
  (konstr-rat (* (zaehler x) (zaehler y))
              (* (nenner x) (nenner y))))

(define (div-rat x y)
  (konstr-rat (* (zaehler x) (nenner y))
              (* (nenner x) (zaehler y))))

(define (gleich-rat? x y)
  (= (* (zaehler x) (nenner y))
     (* (zaehler y) (nenner x))))

(define (drucke-rat z)
  (display (zaehler z))
  (display "/")
  (display (nenner z)))

(define ein-halbes (konstr-rat 1 2))

(define ein-drittel (konstr-rat 1 3))
