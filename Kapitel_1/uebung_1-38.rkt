;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.38 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (ketten-bruch-iter n d k)
  (define (iter n d k ergebnis)
    (if (= k 0)
      ergebnis
      (iter n d (- k 1) (/ (n k) (+ (d k) ergebnis)))))
  (iter n d k 0))

; Die folgende Prozedur berechnet die D_i für diesen Kettenbruch:
; i     = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, ...)
; D_i   = (1, 2, 1, 1, 4, 1, 1, 6, 1, 1,  8,  1,  1,  10, 1,  ...)
(define (e-sequenz i)
  (if (= 2 (remainder i 3))
    (/ (* 2 (+ i 1)) 3)
    1))

(define (berechne-e k)
  (+ 2
     (ketten-bruch-iter (lambda (i) 1.0)
                        e-sequenz
                        k)))
