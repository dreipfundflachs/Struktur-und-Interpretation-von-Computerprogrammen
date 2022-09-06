;;;;;;;;;;;;;;;;;;;;;;;;;;
;  SICP - Abschnitt 2.2  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define x (cons 1 (cons 2 (cons 3 (cons 4 empty)))))

(define y (list 1 2 3 4))

; Beispiele:

; (car x) = 1

; (cdr x) = (2 3 4)

; (cadr x) = 2

; (caar (list (list 1 2) 3)) = 1

; (cddr x) = (3 4)

; (cdddr x) = (4)

; (cddddr x) = null

; (caddr x) = 3

; (list-ref x 0) = 1

; (list-ref x 3) = 4

(define (list-ref elemente n)
  (if (= n 0)
    (car elemente)
    (list-ref elemente (- n 1))))

(define (length elemente)
  (if (null? elemente)
    0
    (+ 1 (length (cdr elemente)))))

(define ungerade (list 1 3 5 7))

(define (length elemente)
  (define (length-iter elemente ergebnis)
    (if (null? elemente)
      ergebnis
      (length-iter (cdr elemente) (+ 1 ergebnis))))
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

(define (skaliere-liste elemente faktor)
  (abb (lambda (x) (* x faktor))
       elemente))

(define test1
  (lambda (x y . z) (+ x y (car z))))

(define test2
  (lambda z (cadr z)))

(define x (cons (list 1 2) (list 3 4)))

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

(define (skaliere-baum baum faktor)
  (map (lambda teilbaum)
       (if (pair? teilbaum)
         (skaliere-baum teilbaum faktor)
         (* faktor teilbaum))
  baum))
