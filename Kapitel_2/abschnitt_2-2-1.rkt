;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  SICP - Abschnitt 2.2.2  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide x y z eins-bis-vier ungerade quadrate)

(define x (cons (cons 1 2) (cons 3 4)))

(define y (cons (cons 1 (cons 2 3)) 4))

(define z (cons 1 (cons 2 (cons 3 (cons 4 empty)))))

(define eins-bis-vier (list 1 2 3 4))

; Beispiele:

(newline)
(display "(car x) = (1, 2)")
(newline)
(car x)
 
(newline)
(display "(cdr x) = (3, 4):")
(newline)
(cdr x)
 
(newline)
(display "(cdar x) = 2:")
(newline)
(cdar x)

(newline)
(display "(cadr x) = 3:")
(newline)
(cadr x)
 
(newline)
(display "(car z) = 1:")
(newline)
(car z)

(newline)
(display "(cdr z) = (2 3 4)")
(newline)
(cdr z)

(newline)
(display "(cadr z) = 2:")
(newline)
(cadr z)

(newline)
(display "(caddr z) = 3:")
(newline)
(caddr z)

(newline)
(display "(cadddr z) = 4:")
(newline)
(cadddr z)

(newline)
(display "(cddddr z) = nil")
(newline)
(cddddr z)

; (caar (list (list 1 2) 3)) = 1
; 
; (cddr x) = (3 4)
; 
; (cdddr x) = (4)
; 
; (cddddr x) = null
; 
; (caddr x) = 3
; 
; (list-ref x 0) = 1
; 
; (list-ref x 3) = 4

(define (list-ref elemente n)
  (if (= n 0)
    (car elemente)
    (list-ref (cdr elemente) (- n 1))))

(define (length elemente)
  (if (null? elemente)
    0
    (+ 1 (length (cdr elemente)))))

(define ungerade (list 1 3 5 7))

(define (length1 elemente)
  (define (length-iter elemente zaehler)
    (if (null? elemente)
      zaehler
      (length-iter (cdr elemente) (+ 1 zaehler))))
  (length-iter elemente 0))

(define quadrate (list 1 4 9 16 25))

(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

(define (skaliere-liste elemente faktor)
  (if (null? elemente)
    null
    (cons (* (car elemente) faktor)
          (skaliere-liste (cdr elemente) faktor))))

(define (abb proz elemente)
  (if (null? elemente)
    null
    (cons (proz (car elemente))
          (abb proz (cdr elemente)))))

(define (skaliere-liste1 elemente faktor)
  (abb (lambda (x) (* x faktor))
       elemente))

(define test1
  (lambda (x y . z) (+ x y (car z))))

(define test2
  (lambda z (cadr z)))

(define (zaehle-blaetter x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (zaehle-blaetter (car x))
                 (zaehle-blaetter (cdr x))))))

(define (skaliere-baum baum faktor)
  (cond ((null? baum) null)
        ((not (pair? baum)) (* faktor baum))
        (else (cons (skaliere-baum (car baum) faktor)
                    (skaliere-baum (cdr baum) faktor)))))

(define liste-1 (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(define (skaliere-baum1 baum faktor)
  (map (lambda (teilbaum)
       (if (pair? teilbaum)
         (skaliere-baum1 teilbaum faktor)
         (* faktor teilbaum)))
  baum))
