;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  SICP - Abschnitt 3.3.1  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(provide x y z)

(define x (mlist (mlist 'a 'b) 'c 'd))

(define y (mlist (mlist 'e 'f) 'c 'd))

(define z (mcons y (mcdr x)))

(set-car! x y)
