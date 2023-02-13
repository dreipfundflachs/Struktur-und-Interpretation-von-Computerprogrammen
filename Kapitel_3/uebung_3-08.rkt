;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 3.08 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide f g)

(define f
  (let ([a 1])
    (lambda (x)
      (begin (set! a (* a x))
             a))))

(define g
  (let ([a 1])
    (lambda (x)
      (begin (set! a (* a x))
             a))))

; Man bemerke, dass f und g dieselbe Definition haben, aber:
(+ (f 0) (f 1))

(+ (g 1) (g 0))
