;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.32 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide untermengen liste0 liste1 liste2 liste3 liste4)

(define (untermengen m)
  (if (null? m)
    (list null)
    (let ((restm (untermengen (cdr m))))
      (append restm (map (lambda (teilmenge)
                           (cons (car m) teilmenge)) restm)))))

(define liste0 null)

(define liste1 (list 1))

(define liste2 (list 1 2))

(define liste3 (list 1 2 3))

(define liste4 (list 1 2 3 4))
