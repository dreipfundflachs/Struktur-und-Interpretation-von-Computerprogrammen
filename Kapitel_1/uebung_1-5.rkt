;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.05 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (p) (p))

(define (test x y)
  (if (= x 0)
    0
    y))

; In normaler Reihenfolge wird (test 0 (p)) so ausgewertet:
; 1. (test 0 (p)) -> {test wird ausgewertet}
; 2. (if (= 0 0) -> {if wird ausgewertet}
;       0
;       (p))
; 3. 0

; In applikativer Reihenfolge wird (test 0 (p)) folgendermaßen ausgewertet:
; 1. (test 0 (p)) -> {(p) wird ausgewertet; 0 ist schon ausgewertet}
; 2. (test 0 (p)) -> {(p) wird ausgewertet; 0 ist schon ausgewertet}
; 3. (test 0 (p)) -> {(p) wird ausgewertet; 0 ist schon ausgewertet}
; ...
; ...
; ...
; Somit ist der Interpretierer in einer endlosen Schleife gefangen worden.
