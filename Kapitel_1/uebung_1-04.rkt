;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.04 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (a_plus_abs_b a b)
  ((if (> b 0) + -) a b))

; Um (a_plus_abs_b a b) auszuwerten ist nach dem Substitutionsmodell Folgendes
; zu tun:
;   1. Die Teilausdrücke a_plus_abs_b, a und b auswerten.
;   2. Die Prozedur, die sich als Wert von a_plus_abs_b ergibt, auf a und b
;      anwenden.
; Im ersten Schritt ergibt sich dann, dass a_plus_abs_b genau dann die Prozedur
; + (bzw. -) als Wert hat, wenn b > 0 (bzw. b <= 0). Also hat
; (a_plus_abs_b a b) auf jeden Fall den Wert a + |b|, wie gewünscht.
