;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.22 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define quadrat (lambda (x) (* x x)))

(define (quadrat-liste elemente)
  (define (iter liste antwort)
    (if (null? liste)
      antwort
      (iter (cdr liste) (cons (quadrat (car liste))
                              antwort))))
  (iter elemente null))

; Die so definierte Prozedur liefert die Antwortliste (der Quadraten der
; ursprünglichen Zahlen) in umgekehrter Reihenfolge, wie man am folgendem
; Beispiel sehen kann:
;
; (quadrat-liste (1 2 3 4)) -> {Auswertung von quadrat-liste}
; (iter (1 2 3 4) null) -> {Auswertung von iter}
; (iter (2 3 4) (cons (quadrat 1) null)) -> {Auswertung von quadrat, cons}
; (iter (2 3 4) (1)) -> {Auswertung von iter}
; (iter (3 4) (cons (quadrat 2) (1))) -> {Auswertung von quadrat, cons}
; (iter (3 4) (4 1)) -> {Auswertung von iter}
; (iter (4) (cons (quadrat 3) (4 1))) -> {Auswertung von quadrat, cons}
; (iter (4) (9 4 1)) -> {Auswertung von iter}
; (iter null (cons (quadrat 4) (9 4 1))) -> {Auswertung von quadrat, cons}
; (iter null (16 9 4 1)) -> {Auswertung von iter}
; (16 9 4 1)

(define (quadrat-liste elemente)
  (define (iter liste antwort)
    (if (null? liste)
      antwort
      (iter (cdr liste)
            (cons antwort (quadrat (car liste))))))
  (iter elemente null))
