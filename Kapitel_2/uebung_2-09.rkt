;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.09 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide breite add-intervall mul-intervall kehr-intervall
         div-intervall sub-intervall drucke-intervall
         konstr-intervall untere-grenze obere-grenze
         I J K)

(define (breite I)
  (/ (- (obere-grenze I) (untere-grenze I)) 2))

; Es seien I = [a, b] und J = [c, d] zwei Intervalle. Dann gelten:

;   (1) I + J = [a + c, b + d] und
;       breite(I + J) = ((b + d) - (a + c)) / 2
;                     = ((b - a) + (d - c)) / 2
;                     = breite(I) + breite(J)
;
;   (2) I - J = [a - d, b - c] und
;       breite(I - J) = ((b - c) - (a - d)) / 2
;                     = ((b - a) + (d - c)) / 2
;                     = breite(I) + breite(J)
;
; Andererseits gelten:

;   (3) Für I = [1, 2], J = [2, 3], ist I * J = [2, 6], dessen Breite
;       gleich 2 ist, während die Breiten von I und J beide gleich 1/2 sind.
;       Für I = [1, 2], K = [4, 5], ist I * K = [4, 10], dessen Breite gleich
;       3 ist, während die Breiten von I und K beide gleich 1/2 sind.
;       Diese Beispiele zeigen also, dass die Breite des Produkts zweier
;       Intervallen keine Funktion dieser Argumentintervalle sein kann.
;
;   (4) Für I = [1, 2], J = [2, 3], ist I / J = [1/3, 1], dessen Breite
;       gleich 1/3 ist, während die Breiten von I und J beide gleich 1/2 sind.
;       Für I = [1, 2], K = [4, 5], ist I / K = [1/5, 1/2], dessen Breite gleich
;       0.15 ist, während die Breiten von I und K beide gleich 1/2 sind.
;       Diese Beispiele zeigen also, dass der Quotient zweier Intervallen 
;       keine Funktion dieser Argumentintervalle sein kann.
;
; Man bemerke dazu, dass in diesen Beispielen das Vorzeichen der Grenzpunkten
; aller Intervallen positiv ist.

; Beispiele:

(define (konstr-intervall a b)
  (cons a b))

(define (untere-grenze I) (car I))

(define (obere-grenze I) (cdr I))

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

; Beispiele von Intervallen:

; I = [1, 2]
(define I (konstr-intervall 1 2))
 
; J = [2, 3]
(define J (konstr-intervall 2 3))

; K = [4, 5]
(define K (konstr-intervall 4 5))
