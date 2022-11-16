;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.18 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide reverse reverse1 gerade)

(define (reverse elemente)
  (define (reverse-iter elemente ergebnis)
    (if (null? elemente)
      ergebnis
      (reverse-iter (cdr elemente) (cons (car elemente) ergebnis))))
  (reverse-iter elemente null))

(define (reverse1 elemente)
  (if (null? elemente)
    null
    (append (reverse1 (cdr elemente)) (list (car elemente)))))

(define gerade (list 0 2 4 6 8 10))
