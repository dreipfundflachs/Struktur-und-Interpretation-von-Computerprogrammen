;;;;;;;;;;;;;;;;;;;;;;;;;;
;  SICP - Abschnitt 2.1  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide ggt konstr-rat zaehler nenner add-rat sub-rat mul-rat div-rat
         gleich-rat? drucke-rat ein-halbes ein-drittel) 

(define (ggt a b)
  (if (= b 0)
    a
    (ggt b (modulo a b))))

(define (konstr-rat n d)
  (let ((g (ggt n d)))
  (cons (/ n g) (/ d g))))

(define (zaehler z) (car z))

(define (nenner z) (cdr z))

(define (add-rat x y)
  (konstr-rat (+ (* (zaehler x) (nenner y))
                 (* (zaehler y) (nenner x)))
              (* (nenner x) (nenner y))))

(define (sub-rat x y)
  (konstr-rat (- (* (zaehler x) (nenner y))
                 (* (zaehler y) (nenner x)))
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

(define x (cons 1 2))

(define y (cons 3 4))

(define z (cons x y))
