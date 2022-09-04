;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.42 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (komposition g f)
  (lambda (x) (g (f x))))

(define (doppelt f) (komposition f f))

; Beispiel:

(define (quadrat x) (* x x))

(define (inc x) (+ x 1))

(display ((komposition quadrat inc) 6))

(newline)
