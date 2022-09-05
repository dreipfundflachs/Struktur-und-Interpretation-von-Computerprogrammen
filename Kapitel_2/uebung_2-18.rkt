;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.18 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reverse elemente)
  (define (reverse-iter elemente ergebnis)
    (if (null? elemente)
      ergebnis
      (reverse-iter (cdr elemente) (cons (car elemente) ergebnis))))
  (reverse-iter elemente null))
