;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.02 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Implementierung von Punkten

(define (konstr-punkt x y) (cons x y))

(define (x-koord p) (car p))

(define (y-koord p) (cdr p))

(define (drucke-punkt p)
  (display "(")
  (display (x-koord p))
  (display " , ")
  (display (y-koord p))
  (display ")"))

(define (gleiche-punkte? p q)
  (and (= (x-koord p) (x-koord q))
       (= (y-koord p) (y-koord q))))

; Implementierung von Strecken

(define (konstr-strecke p q) (cons p q))

(define (anfangs-punkt s) (car s))

(define (end-punkt s) (cdr s))

(define (mittelwert a b) (/ (+ a b) 2))

(define (mittel-punkt-strecke s)
  (konstr-punkt
    (mittelwert (x-koord (anfangs-punkt s)) (x-koord (end-punkt s)))
    (mittelwert (y-koord (anfangs-punkt s)) (y-koord (end-punkt s)))))

(define (drucke-strecke s)
  (display "[")
  (drucke-punkt (anfangs-punkt s))
  (display " , ")
  (drucke-punkt (end-punkt s))
  (display "]"))


; Beispiele:

(define a (konstr-punkt 1 1))

(define b (konstr-punkt 3 3))

(define s (konstr-strecke a b))

(define m (mittel-punkt-strecke s))
