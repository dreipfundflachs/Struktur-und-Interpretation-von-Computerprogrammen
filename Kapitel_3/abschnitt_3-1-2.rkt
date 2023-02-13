;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  SICP - Abschnitt 3.1.2  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(define zufall
  (let ([x zufall-init])
    (lambda ()
      (set! x (zufall-aktuell x))
      x)))

(define (schaetzwert-pi versuche)
  (sqrt (/ 6 (monte-carlo versuche cesaro-test))))

(define (cesaro-test)
  (= ggt (zufall) (zufall) ))

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
