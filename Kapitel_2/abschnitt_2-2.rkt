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
