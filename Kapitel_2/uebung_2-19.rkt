;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.19 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pfennig-muenzen (list 50 10 5 2 1))

(define uk-muenzen (list 100 50 20 10 5 2 1 0.5))

(define us-muenzen (list 50 25 10 5 1))

(define erster-nennwert car)

(define ausser-erstem-nennwert cdr)

(define keine-mehr? null?)

(define (wg betrag muenzarten)
  (cond ((= betrag 0) 1)
        ((or (< betrag 0) (keine-mehr? muenzarten)) 0)
        (else
          (+ (wg betrag (ausser-erstem-nennwert muenzarten))
             (wg (- betrag (erster-nennwert muenzarten)) muenzarten)))))

; Die Reihenfolge der Münzarten beinflusst nicht die Antwort, die wg liefert,
; weil die Summe von ganzen Zahlen kommutativ ist. Z. B. weil
;   100 = 50 + 25 + 10 + 10 + 5,
; gilt auch
;   100 = 5 + 10 + 10 + 50 + 25.
