;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.15 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (kubik x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (kubik x))))

(define (sinus winkel)
  (if (< (abs winkel) 0.1)
    winkel
    (p (sinus (/ winkel 3.0)))))

; (a)
; Um sin(12.15) auszuwerten, wird die Prozedur p fünf mal angewendet, wie
; man folgendermaßen sieht:
;   (sinus 12.15) ->
;   p (sinus 4.05) ->
;   p (p (sinus 1.35)) ->
;   p (p (p (sinus 0.45))) -> 
;   p (p (p (p (sinus 0.15)))) ->
;   p (p (p (p (p (sinus 0.05))))) ->
;   p (p (p (p (p 0.05)))) ->
;   -0.39980345741334
;   

; (b)
; Sowohl der Speicherbedarf als auch die Anzahl der Schritte, die notwendig
; sind, um (sin a) auszuwerten, haben eine Größenordnung Θ(log_3 a). In der
; Tat, um die Rechnung durchzuführen, braucht man ungefähr log_3 (a / 0.1)
; Schritte und die gleiche Anzahl von Zwischenwerte zu speichern.
