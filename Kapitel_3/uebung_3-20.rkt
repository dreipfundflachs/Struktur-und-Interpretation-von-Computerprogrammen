;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 3.20 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide mcons mcar mcdr set-car! set-cdr!)

(define (mcons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (zuteilen n)
    (cond ([eq? n 'car] x)
          ([eq? n 'cdr] y)
          ([eq? n 'set-car!] set-x!)
          ([eq? n 'set-cdr!] set-y!)
          (else (error "Operation nicht definiert -- CONS" n))))
  zuteilen)

(define (mcar z) (z 'car))
(define (mcdr z) (z 'cdr))
(define (set-car! z neuer-wert)
  ((z 'set-car!) neuer-wert)
  'ok!)
(define (set-cdr! z neuer-wert)
  ((z 'set-cdr!) neuer-wert)
  'ok!)

; Beispiele:
(define x (mcons 1 2))
(define z (mcons x x))
(set-car! (mcdr z) 17)
(mcar x)

