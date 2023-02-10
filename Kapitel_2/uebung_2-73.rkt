;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.73 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide ableitung
         installiere-summe-paket
         installiere-produkt-paket
         installiere-potenz-paket
         operator operanden
         konstr-summe konstr-produkt konstr-potenz
         ableitung
         variable? gleiche-variable? =number?)

; (a)
(define (ableitung ausdr var)
  (cond ([number? ausdr] 0)
        ([variable? ausdr]
         (if (gleiche-variable? ausdr var)
           1
           0))
        (else ((get 'ableitung (operator ausdr)) (operanden ausdr) var))))

(define (operator ausdr) (car ausdr))

(define (operanden ausdr) (cdr ausdr))

; Wir können die Prädikate 'number?' und 'variable?' nicht in den
; datengesteuerten Verteiler aufnehmen, weil die Ausdrücke, die sie genügen,
; nur aus einem einzigem Operand bestehen und mit keinem entsprechenden
; Operator assoziiert sind.

; (b)
(define (installiere-summe-paket)
  ; Interne Prozeduren:
  (define (konstr-summe-sum a1 a2)
    (cond ([=number? a1 0] a2)
          ([=number? a2 0] a1)
          ([and (number? a1) (number? a2)] (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (addend s) (cadr s))
  (define (augend s) (caddr s))
  (define (ableitung-sum operanden var)
    (konstr-summe (ableitung (addend operanden) var)
                  (ableitung (augend operanden) var)))
  ; Schnittstelle zum übrigen System:
  (put 'ableitung '+ ableitung-sum)
  (put 'konstr-summe '+ konstr-summe-sum)
  'fertig)

(define (installiere-produkt-paket)
  ; Interne Prozeduren:
  (define (konstr-produkt-prod m1 m2)
    (cond ([=number? m1 0] 0)
          ([=number? m1 1] m2)
          ([=number? m2 0] 0)
          ([=number? m2 1] m1)
          ([and (number? m1) (number? m2)] (* m1 m2))
          (else (list '* m1 m2))))
  (define (multiplikator p) (cadr p))
  (define (multiplikand p) (caddr p))
  (define (ableitung-prod operanden var)
    (konstr-summe (konstr-produkt (ableitung (multiplikator operanden) var)
                                  (multiplikand operanden))
                  (konstr-produkt (multiplikator operanden)
                                  (ableitung (multiplikand operanden) var))))
  ; Schnittstelle zum übrigen System:
  (put 'ableitung '* ableitung-prod)
  (put 'konstr-produkt '* konstr-produkt-prod)
  'fertig)

(define konstr-summe
  (get 'konstr-summe '+))

(define konstr-produkt
  (get 'konstr-produkt '*))

(define (variable? x) (symbol? x))

(define (gleiche-variable? x y)
  (and (variable? x) (variable? y) (eq? x y)))

(define (=number? ausdr zahl)
  (and (number? ausdr) (= ausdr zahl)))

; (c)

(define (installiere-potenz-paket)
  ; Interne Prozeduren:
  (define (konstr-potenz-pot basis exponent)
    (cond ([=number? exponent 0] 1)
          ([=number? exponent 1] basis)
          ([=number? basis 0] 0)
          ([=number? basis 1] 1)
          (else (list '** basis exponent))))
  (define (basis potenz) (cadr potenz))
  (define (exponent potenz) (caddr potenz))
  (define (ableitung-potenz operanden var)
    (konstr-produkt
      (konstr-produkt (exponent ausdr)
                      (konstr-potenz (basis ausdr)
                                     (- (exponent ausdr) 1)))
      (ableitung (basis ausdr) var)))
  ; Schnittstelle zum übrigen System:
  (put 'ableitung '** ableitung-potenz)
  (put 'konstr-potenz '** konstr-potenz-pot)
  'fertig)

(define konstr-potenz
  (get 'konstr-potenz '**))

; (d) Wir müssten dann die Reihenfolge der Indizierungsargumenten in jedem
; Aufruf von 'put' invertieren.
