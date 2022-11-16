;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.22 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide quadrat quadrat-liste quadrat-liste1 zahlen)

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
; (quadrat-liste (1 2 3 4))                   {Auswertung von quadrat-liste}
; = (iter (1 2 3 4) null)                     {Auswertung von iter}
; = (iter (2 3 4) (cons (quadrat 1) null))    {Auswertung von quadrat, cons}
; = (iter (2 3 4) (1))                        {Auswertung von iter}
; = (iter (3 4) (cons (quadrat 2) (1)))       {Auswertung von quadrat, cons}
; = (iter (3 4) (4 1))                        {Auswertung von iter}
; = (iter (4) (cons (quadrat 3) (4 1)))       {Auswertung von quadrat, cons}
; = (iter (4) (9 4 1))                        {Auswertung von iter}
; = (iter null (cons (quadrat 4) (9 4 1)))    {Auswertung von quadrat, cons}
; = (iter null (16 9 4 1))                    {Auswertung von iter}
; = (16 9 4 1)

(define (quadrat-liste1 elemente)
  (define (iter liste antwort)
    (if (null? liste)
      antwort
      (iter (cdr liste)
            (cons antwort
                  (quadrat (car liste))))))
  (iter elemente null))

; Die so definierte Prozedur liefert die falsche Antwort, weil in Lisp eine
; Liste durch wiederholte Einfügung eines Elements am _Anfang_ der Liste
; aufgebaut wird. Im Gegensatz dazu, fügt diese Prozedur neue Elemente immer am
; _Ende_ der vorherigen Sequenz hinzu.
; 
; Den Unterschied kann man leicht am folgendem Beispiel beobachten

; (1 4 9 16) = (cons 1 (cons 4 (cons 9 (cons 16 null))))

; /= (cons (cons (cons (cons null 1) 4) 9) 16) = ((((() 1) 4) 9) 16)

(define zahlen (list 1 2 3 4))

(map quadrat zahlen)

(quadrat-liste zahlen)

(quadrat-liste1 zahlen)
