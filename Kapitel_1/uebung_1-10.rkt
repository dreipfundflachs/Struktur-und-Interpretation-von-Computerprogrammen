;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.10 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

; Die Funktion A hat also die folgende Eigenschaften (man beachte dazu die
; Folge, in der diese Eigenschaften aufgeführt werden):
; (i)   A(x, 0) = 0;
; (ii)  A(0, y) = 2y;
; (iii) A(x, 1) = 2;
; (iv)  A(x, y) = A(x - 1, A(x, y - 1))
;
; (a)
;   Antwort: A(1, 10) = 2^10.
;   Beweis:
;     A(1, 10)
;   = A(0, A(1, 9))
;   = 2 A(1, 9)
;   = 2 A(0, A(1, 8))
;   = 4 A(1, 8)
;   = ... 
;   = (2^15) A(1, 1)
;   = (2^15) 2
;   = 2^16

;   Allgemeiner kann man mithilfe
;   vollständiger Induktion beweisen, dass A(1, n) = 2^n für n > 0 gilt.   
;
; (b)
;   Antwort: A(2, 4) = 2^16 = 65536.
;   Beweis:
;     A(2, 4)
;   = A(1, A(2, 3)) {nach (iv)}
;   = A(1, A(1, A(2, 2))) {nach (iv)}
;   = A(1, A(1, A(1, A(2, 1)))) {nach (iv)}
;   = A(1, A(1, A(1, 2)))) {nach (iii)}
;   = A(1, A(1, 4)) {nach (a)}
;   = A(1, 16) {nach (a)}
;   = 2^16 {nach (a)}
;   = 65536
;
; (c)
;   Antwort: A(3, 3) = 2^16 = 65536
;   Beweis:
;     A(3, 3)
;   = A(2, A(3, 2)) {nach (iv)}
;   = A(2, A(2, A(3, 1))) {nach (iv)}
;   = A(2, A(2, 2))) {nach (iii)}
;   = A(2, A(1, A(2, 1))) {nach (iv)}
;   = A(2, A(1, 2)) {nach (iii)}
;   = A(2, A(0, A(1, 1))) {nach (iv)}
;   = A(2, 2A(1, 1)) {nach (ii)}
;   = A(2, 4) {nach (iii)}
;   = 2^16 {nach (b)}
;
; Allgemeiner kann man mithilfe vollständigker Induktion beweisen, dass
;       A(2, n) = 2^(2^(2^(...))) (n Kopien von 2)
; für alle n > 0. Zum Beispiel:
;   A(2, 1) = 2
;   A(2, 2) = 2^2 = 4
;   A(2, 3) = 2^(2^2) = 2^4 = 16
;   ...
;   A(2, n) = 2^(A(2, n - 1))
