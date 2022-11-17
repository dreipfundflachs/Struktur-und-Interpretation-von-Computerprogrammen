;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.28 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide x y z blaetter)

(define x (list (list 1 2) (list 3 4)))

(define y (list 0 (list 1 2) (list 3 4) 5))

(define z (list x x))

(define (blaetter liste)
  (cond ((null? liste) null)
        ((not (pair? liste)) (list liste))
        (else (append (blaetter (car liste)) (blaetter (cdr liste))))))

; Beispiele:

(display (blaetter x))
(newline)
(display (blaetter x))
(newline)

(newline)
(display y)
(newline)
(display (blaetter y))
(newline)

(newline)
(display z)
(newline)
(display (blaetter z))
(newline)
