;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 3.05 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide schaetzwert-integral zufall-bereich monte-carlo
         quadrat beispiel-praedikat)

(define (schaetzwert-integral praedikat versuche x1 x2 y1 y2)
  (let ([experiment (lambda () (integral-test praedikat x1 x2 y1 y2))])
    (monte-carlo versuche experiment)))

(define (integral-test praedikat x1 x2 y1 y2)
  (and (praedikat (zufall-bereich x1 x2)
                  (zufall-bereich y1 y2))))

(define (zufall-bereich unten oben)
  (let ((bereich (- oben unten)))
    (+ unten (* (random) bereich))))

(define (monte-carlo versuche experiment)
  (define (iter versuche-uebrig versuche-erfolgreich)
    (cond ([= versuche-uebrig 0]
           (/ versuche-erfolgreich versuche))
          ([experiment]
           (iter (- versuche-uebrig 1)
                 (+ versuche-erfolgreich 1)))
          (else
            (iter (- versuche-uebrig 1)
                  versuche-erfolgreich))))
  (iter versuche 0))

; Beispiel:
(define (quadrat t) (* t t))
(define (beispiel-praedikat x y)
  (<= (+ (quadrat (- x 5)) (quadrat (- y 7))) 9))
(display "Schätzung von pi:")
(newline)
(* (schaetzwert-integral beispiel-praedikat 1000000 2.0 8.0 4.0 10.0) 4.0)
