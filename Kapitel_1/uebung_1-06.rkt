;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.06 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (neues_if praedikat then_klausel else_klausel)
  (cond (praedikat then_klausel)
        (else else_klausel)))

(neues_if (= 2 3) 0 5)

(neues_if (= 1 1) 0 5)

(define (wurzel_iter schaetzwert x)
  (neues_if (gut_genug? schaetzwert x)
            schaetzwert
            (wurzel_iter (verbessern schaetzwert x)
                         x)))

; Weil der Lisp-Interpretierer die applikative Reihenfolge benutzt um Ausdrücke
; auszuwerten, würde Alyssa versuchen, mit diesem Programm eine Quadratwurzel
; zu berechnen, dann würde der Interpretierer in einer endlosen Schleife
; hineingehen. Tatsächlich, muss die else_klausel immer erneut ausgewertet
; werden, sogar wenn der erste Schätzwert gut genug ist. 
;
; Das Problem hier liegt beim Tat, dass cond, die eine Sonderform ist, nicht
; direkt ausgerufen wird. Stattdessen ruft der Programm eine Prozedur neues_if
; auf mit einem _rekursiven_ Argument (wurzel_iter (verbessern schaetzwert
; x)). Im Kontrast zu cond, muss diesen Argument dann immer wieder ausgewertet
; werden.

(define (abs x)
  (if (> x 0) x (- x)))

(define (verbessern schaetzwert x)
  (mittelwert schaetzwert (/ x schaetzwert)))

(define (mittelwert x y)
  (/ (+ x y) 2))

(define (gut_genug? schaetzwert x)
  (< (abs (- (quadrat schaetzwert) x))
     0.001))

(define (wurzel x)
  (wurzel_iter 1.0 x))

(define (quadrat x) (* x x))
