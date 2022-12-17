;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.25 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide liste1 liste2 liste3)

(define liste1 (list 1 3 (list 5 7) 9))

(define liste2 (list (list 7)))

(define liste3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(car (cdaddr liste1))

(caar liste2)

(cadr (cadr (cadr (cadr (cadr (cadr liste3))))))
