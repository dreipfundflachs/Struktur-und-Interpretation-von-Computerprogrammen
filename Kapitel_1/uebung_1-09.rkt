;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.09 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (inc n) (+ n 1))

(define (dec n) (- n 1))

; Diese rekursive Prozedur zur Berechnung einer Summe erzeugt einen linear
; rekursiven Prozess:
(define (summe_r a b)
  (if (= a 0)
    b
    (inc (summe_r (dec a) b))))

; Im Gegensatz dazu, erzeugt die folgende rekursive Prozedur zur Berechnung
; einer Summe erzeugt einen linear iterativen Prozess:
(define (summe_i a b)
  (if (= a 0)
    b
    (summe_i (dec a) (inc b))))

; Als Beispeil betrachten wir die Auswertung von (+ 4 5) mithilfe dieser zwei
; Prozeduren:
;
; (summe_r 4 5) ->
; (inc (summe_r (dec 4) 5)) ->
; (inc (summe_r 3 5)) ->
; (inc (inc (summe_r (dec 3) 5))) ->
; (inc (inc (summe_r 2 5))) ->
; (inc (inc (inc (summe_r (dec 2) 5)))) ->
; (inc (inc (inc (summe_r 1 5)))) ->
; (inc (inc (inc (inc (summe_r (dec 1) 5))))) ->
; (inc (inc (inc (inc) (summe_r 0 5)))) ->
; (inc (inc (inc (inc 5)))) ->
; (inc (inc (inc 6))) ->
; (inc (inc 7)) ->
; (inc 8) ->
; 9
;
;
; (summe_i 4 5) ->
; (summe_i (dec 4) (inc 5)) ->
; (summe_i 3 6) ->
; (summe_i (dec 3) (inc 6)) ->
; (summe_i 2 6) ->
; (summe_i (dec 2) (inc 6)) ->
; (summe_i 1 7) ->
; (summe_i (dec 1) (inc 8)) ->
; (summe_i 0 9) ->
; 9
