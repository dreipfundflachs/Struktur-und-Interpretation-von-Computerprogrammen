;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.21 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (quadrat-liste elemente)
  (if (null? elemente)
    null
    (cons ((lambda (x) (* x x)) (car elemente))
          (quadrat-liste (cdr elemente)))))

; 'abb' ist in Scheme als 'map' vordefiniert.
(define (quadrat-liste elemente)
  (map (lambda (x) (* x x)) elemente))

(define quadrat-liste
  (lambda (zahlen) (map (lambda (x) (* x x)) zahlen)))

