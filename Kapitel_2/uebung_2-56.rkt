;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.56 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide variable? gleiche-variable?  =number?
         summe? addend augend konstr-summe
         produkt? multiplikator multiplikand konstr-produkt
         potenz? basis exponent konstr-potenz
         ableitung)

(define (potenz? ausdr)
  (and (pair? ausdr) (eq? (car ausdr) '**)))

(define (basis potenz) (cadr potenz))

(define (exponent potenz) (caddr potenz))

(define (konstr-potenz basis exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) basis)
        ((=number? basis 0) 0)
        ((=number? basis 1) 1)
        (else (list '** basis exponent))))

(define (ableitung ausdr var)
  (cond ((number? ausdr) 0)
        ((variable? ausdr)
         (if (gleiche-variable? ausdr var)
           1
           0))
        ((summe? ausdr)
         (konstr-summe (ableitung (addend ausdr) var)
                       (ableitung (augend ausdr) var)))
        ((produkt? ausdr)
         (konstr-summe
           (konstr-produkt (multiplikator ausdr)
                           (ableitung (multiplikand ausdr) var))
           (konstr-produkt (ableitung (multiplikator ausdr) var)
                           (multiplikand ausdr))))
        ((potenz? ausdr)
         (konstr-produkt
           (konstr-produkt (exponent ausdr)
                           (konstr-potenz (basis ausdr)
                                          (- (exponent ausdr) 1)))
           (ableitung (basis ausdr) var)))
        (else
          (error "Unbekannter Ausdruck -- ABLEITUNG" ausdr))))

(define (variable? x) (symbol? x))

(define (gleiche-variable? x y)
  (and (variable? x) (variable? y) (eq? x y)))

(define (=number? ausdr zahl)
  (and (number? ausdr) (= ausdr zahl)))

(define (konstr-summe a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2) (+ a1 a2)))
        (else (list '+ a1 a2))))

(define (konstr-produkt m1 m2)
  (cond ((=number? m1 0) 0)
        ((=number? m1 1) m2)
        ((=number? m2 0) 0)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (summe? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (produkt? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplikator p) (cadr p))

(define (multiplikand p) (caddr p))

; Beispiele:

; Ableitung von (x + 3) = 1
(newline)
'(+ x 3)
(ableitung '(+ x 3) 'x)
(newline)

; Ableitung von xy = y
'(* x y)
(ableitung '(* x y) 'x)
(newline)

; Ableitung von xy(x + 3) nach x = y(x + 3) + xy
'(* (* x y) (+ x 3))
(ableitung '(* (* x y) (+ x 3)) 'x)
(newline)

; Ableitung von x**3 nach x = 3x**2
'(** x 3)
(ableitung '(** x 3) 'x)
(newline)
