;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.26 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (potmod basis exponent m)
  (cond ((= exponent 0) 1)
        ((gerade? exponent)
         (remainder (* (potmod basis (/ exponent 2) m)
                       (potmod basis (/ exponent 2) m))
                    m))
        (else (remainder (* basis (potmod basis (- exponent 1) m))
                         m))))

(define (gerade? n) (= 0 (remainder n 2)))

; Das Problem mit dieser Prozedur ist, dass jeder Aufruf von potmod mit einem
; geradem Exponent _zwei_ neue Aufrufe von (potmod basis (/ exponent 2))
; erzeugt. Dadurch braucht die Prozedur:
;   (1) Zwei Berechnungen, um (potmod b 2) zu bestimmen, denn b^1 wird zweimal
;       berechnet.
;   (2) Vier Berechnungen, um (potmod b 4) zu bestimmen, denn b^1 wird viermal
;       berechnet. In der Tat, fūhrt jeder der zwei Aufrufen von (potmod b 2)
;       zu zwei Berechnungen von (potmod b 1).)
;   ...
;   (n) 2^n Berechnungen, um (potmod b 2^n) zu bestimmen.
; Deshalb hat diese Version von potmod die Grössenordnung O(exponent).

; Als Beispiel berechne man:

; (potmod 3 10000000 7)
