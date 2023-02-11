;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.84 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide hoehere-typen hoeher-als)

(define (hoehere-typen typ)
; Liefert die Liste aller Typen, die gleich 'typ' sind oder, die sich auf einer
; höheren Stufe des Turmes befinden.
  (define (iter typ ergebnis)
    (let ([naechster-typ erhoehen typ])
      (if (not naechster-typ)
        ergebnis
        (iter naechster-typ (cons typ ergebnis)))))
  (iter typ null))

(define (hoeher-als typ1 typ2)
  (if (member typ2 (hoehere-typen typ1))
    #t
    #f))

(define (max-typ typen)
  (cond ([= (length typen) 1] (car typen))
        ([hoeher-als (cadr typen) (car typen)]
         max-typ (cdr typen))
        (else (max-typ (cons (car typen) (cddr typen))))))

; Implementierung einer allgemeinen Operations-Prozedur:
(define (anwenden-generisch op . args)
  (let ([typ-etiketten (map typ-etikett args)])
    (let ([proc (get op typ-etiketten)])
      (if proc
        (apply proc (map inhalt args))
        (error "Keine Methode für diese Typen -- ANWENDEN-GENERISCH"
               (list op typ-etiketten))))))
