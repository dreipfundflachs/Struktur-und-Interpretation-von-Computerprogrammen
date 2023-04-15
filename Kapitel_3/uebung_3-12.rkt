;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 3.12 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(provide x y z w
         append!
         letztes-paar)

; (define (append x y)
;   (if (null? x)
;     y
;     (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (letztes-paar x) y)
  x)

(define (letztes-paar x)
  (if (null? (mcdr x))
    x
    (letztes-paar (mcdr x))))

(define x (mlist 'a 'b))
; {a b}

(define y (mlist 'c 'd))
; {c d}

(define z (mappend x y))
; {a b c d}

(display (mcdr x))
; {b}
(newline)

(define w (append! x y))

(display w)
; {a b c d}
(newline)

(display (mcdr x))
; {b c d}
(newline)
