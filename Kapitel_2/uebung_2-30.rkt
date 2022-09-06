;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.30 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (quadrat x) (* x x))

(define baum (list 0 (list 1 2) (list 3 4) 5))

(define (quadrat-baum baum)
  (cond ((null? baum) null)
        ((pair? baum)
         (cons (quadrat-baum (car baum)) (quadrat-baum (cdr baum))))
        (else (quadrat baum))))

(define (abb-quadrat-baum baum)
  (lambda (teilbaum)
    (if (pair? teilbaum)
      (map abb-quadrat-baum teilbaum)
      (quadrat teilbaum)))
  baum)
