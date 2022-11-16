;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.14 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide add-intervall mul-intervall kehr-intervall
         div-intervall sub-intervall drucke-intervall
         konstr-intervall untere-grenze obere-grenze
         konstr-mittel-prozent mittel prozent breite
         par1 par2 A B)

; Implementierung von Intervallen:

(define (konstr-intervall a b)
  (cons a b))

(define (untere-grenze I) (car I))

(define (obere-grenze I) (cdr I))

; Implementierung von Intervallen durch den Mittelpunkt und die Toleranz:

(define (konstr-mittel-prozent m p)
  (konstr-intervall (- m (* p m)) (+ m (* p m))))

(define (mittel I)
  (/ (+ (untere-grenze I) (obere-grenze I)) 2))

(define (prozent I)
  (/ (breite I) (mittel I)))

(define (breite I)
  (/ (- (obere-grenze I) (untere-grenze I)) 2))

; Implementierung einer "Intervallarithmetik":

(define (add-intervall I J)
  (konstr-intervall (+ (untere-grenze I) (untere-grenze J))
                    (+ (obere-grenze I) (obere-grenze J))))

(define (mul-intervall I J)
  (let ((p1 (* (untere-grenze I)    (untere-grenze J)))
        (p2 (* (untere-grenze I)    (obere-grenze J)))
        (p3 (* (obere-grenze I)     (untere-grenze J)))
        (p4 (* (obere-grenze I)     (obere-grenze J))))
    (konstr-intervall (min p1 p2 p3 p4)
                      (max p1 p2 p3 p4))))

(define (kehr-intervall I)
  (konstr-intervall (/ 1.0 (obere-grenze I))
                    (/ 1.0 (untere-grenze I))))

(define (div-intervall I J)
  (mul-intervall I (kehr-intervall J)))

(define (sub-intervall I J)
  (konstr-intervall
    (- (untere-grenze I)    (obere-grenze J))
    (- (obere-grenze I)     (untere-grenze J))))

(define (drucke-intervall I)
  (display "[")
  (display (untere-grenze I))
  (display " , ")
  (display (obere-grenze I))
  (display "]"))

(define (par1 I J)
  (div-intervall (mul-intervall I J)
                 (add-intervall I J)))

(define (par2 I J)
  (let ((eins (konstr-intervall 1 1)))
    (div-intervall eins
                   (add-intervall (div-intervall eins I)
                                  (div-intervall eins J)))))

; Beispiele von Intervallen:

; A = [1, 2]
(define A (konstr-intervall 1 2))
 
; B = [2, 3]
(define B (konstr-intervall 2 3))

; Man berechne:

(newline)
(display "Harmonische Summe von A und B, erste Methode:")
(newline)
(drucke-intervall (par1 A B))
(newline)

(newline)
(display "Harmonische Summe von A und B, zweite Methode:")
(newline)
(drucke-intervall (par2 A B))
(newline)


(newline)
(display "Quotient von A durch A:")
(newline)
(drucke-intervall (div-intervall A A))
(newline)


(newline)
(display "Quotient von A durch B:")
(newline)
(drucke-intervall (div-intervall A B))
(newline)
