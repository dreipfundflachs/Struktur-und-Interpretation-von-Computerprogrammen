;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.57 - SICP  ;;;;
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
  (cond ([number? ausdr] 0)
        ([variable? ausdr]
         (if (gleiche-variable? ausdr var)
           1
           0))
        ([summe? ausdr]
         (konstr-summe (list (ableitung (addend ausdr) var)
                             (ableitung (augend ausdr) var))))
        ([produkt? ausdr]
         (konstr-summe (list 
                         (konstr-produkt
                           (list (multiplikator ausdr)
                                 (ableitung (multiplikand ausdr) var)))
                         (konstr-produkt
                           (list (ableitung (multiplikator ausdr) var)
                                 (multiplikand ausdr))))))
        ([potenz? ausdr]
         (konstr-produkt (list (exponent ausdr)
                               (konstr-potenz (basis ausdr)
                                              (- (exponent ausdr) 1))
                               (ableitung (basis ausdr) var))))
        (else
          (error "Unbekannter Ausdruck -- ABLEITUNG" ausdr))))

(define (variable? x) (symbol? x))

(define (gleiche-variable? x y)
  (and (variable? x) (variable? y) (eq? x y)))

(define (=number? ausdr zahl)
  (and (number? ausdr) (= ausdr zahl)))

(define (konstr-summe as)
  (cond ([null? as] 0)
        ([not (pair? as)] as)
        ([= (length as) 1] (car as))
        ([=number? (car as) 0] (konstr-summe (cdr as)))
        ([=number? (konstr-summe (cdr as)) 0] (car as))
        ([and (number? (car as)) (number? (konstr-summe (cdr as)))]
         (+ (car as) (konstr-summe (cdr as))))
        (else (cons '+ as))))

(define (konstr-produkt ps)
  (cond ([null? ps] 1)
        ([not (pair? ps)] ps)
        ([= (length ps) 1] (car ps))
        ([=number? (car ps) 1] (konstr-produkt (cdr ps)))
        ([=number? (konstr-produkt (cdr ps)) 1] (car ps))
        ([=number? (car ps) 0] 0)
        ([=number? (konstr-produkt (cdr ps)) 0] 0)
        ([and (number? (car ps)) (number? (konstr-produkt (cdr ps)))]
         (* (car ps) (konstr-produkt (cdr ps))))
        (else (cons '* ps))))

(define (summe? ausdr)
  (or (and (pair? ausdr) (eq? (car ausdr) '+))
      (not (pair? ausdr))))

(define (addend s)
  (if (not (pair? s))
    s
    (cadr s)))

(define (augend s)
  (if (not (pair? s))
    0
    (konstr-summe (caddr s))))

(define (produkt? ausdr)
  (or (and (pair? ausdr) (eq? (car ausdr) '*))
      (not (pair? ausdr))))

(define (multiplikator p)
  (if (not (pair? p))
    p
    (cadr p)))

(define (multiplikand p)
  (if (not (pair? p))
    1
    (konstr-produkt (caddr p))))

; Beispiele:

(define s '(+ x 3 x))
(summe? s)
(konstr-summe '(x 3 x))
(konstr-summe '(3 x))
; Ableitung von (x + 3) = 1
(newline)
'(+ (x 3))
(ableitung '(+ x 3 x) 'x)
(newline)

; Ableitung von xy = y
(konstr-produkt '(x y))
(ableitung '(* x y) 'x)
(newline)

; Ableitung von xy(x + 3) nach x = y(x + 3) + xy
(ableitung (konstr-produkt '(x y (+ x 3))) 'x)
(newline)

; Ableitung von x**3 nach x = 3x**2
'(** x 3)
(ableitung '(** x 3) 'x)
(newline)
