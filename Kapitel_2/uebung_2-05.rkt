;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.05 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cons a b) (* (expt 2 a) (expt 3 b)))

(define (car z) (extrahiere 2 z))

(define (cdr z) (extrahiere 3 z))

(define (extrahiere k n)
  (if (= 0 (modulo n k))
    (+ 1 (extrahiere k (/ n k)))
    0))

; Beispiele:

(define z (cons 4 5))

(define a (car z))

(define b (cdr z))

; Beweis
;
; Wir beweisen nur, dass cdr (cons a b) für alle positive ganze Zahlen gilt.
; Der Beweis, dass auch car (cons a b) gilt, ist analog.
;
; 1. cdr (cons a b) -> {Auswertung des Rumpfes von 'cdr' und 'cons'}
; 2. (extrahiere 3 (* (expt 2 a) (expt 3 b))) -> {Auswertung des zweiten 
;                                                 Arguments}
; 3. (extrahiere 3 (2^a 3^b)) -> {Anwendung von 'extrahiere' auf ihre Argumente}
; 4. (+ 1 (extrahiere 3 (2^a 3^(b - 1)))) -> {Anwendung von 'extrahiere'}
; .
; .
; .
; (4 + b). (+ 1 (+ 1 (+ 1 ...(extrahiere 3 (2^a))))) -> {Anw. von 'extrahiere'}
; (4 + b + 1). (+ 1 (+ 1 (+ 1 ...(+ 1 0))))
; (4 + b + 2). b
