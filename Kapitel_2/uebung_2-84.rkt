;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.84 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide hoehere-typen hoeher-als gleicher-typ max-typ
         anwenden-generisch)

(define (hoehere-typen typ)
; Liefert die Liste aller Typen, die gleich 'typ' sind oder, die sich auf einer
; höheren Stufe des Turmes befinden.
  (define (iter typ ergebnis)
    (let ([naechster-typ (erhoehen typ)])
      (if (not naechster-typ)
        ergebnis
        (iter naechster-typ (cons typ ergebnis)))))
  (iter typ null))

(define (hoeher-als typ1 typ2)
  (if (member typ2 (hoehere-typen typ1))
    #t
    #f))

(define (gleicher-typ typ1 typ2) eq?)

(define (max-typ typen)
  (cond ([= (length typen) 1] (car typen))
        ([hoeher-als (cadr typen) (car typen)]
         max-typ (cdr typen))
        (else (max-typ (cons (car typen) (cddr typen))))))

(define (anwenden-generisch op . args)
  (let ([typen (map typ-etikett args)])
    (let [proc (get op typen)]
      (if proc
        (apply proc (map inhalt args))
        (if (= (length args) 2)
          (let ([maximum (max-typ typen)]
                [typ1 (car typen)]
                [typ2 (cadr typen)]
                [arg1 (inhalt (car args))]
                [arg2 (inhalt (cadr args))])
            (cond ([(gleicher-typ typ1 typ2)]
                   error
                   "Keine Methode für diese Typen -- ANWENDEN-GENERISCH"
                   (list op typen))
                  ([hoeher-als typ1 typ2]
                   (anwenden-generisch op (list arg1 (erhoehen arg2))))
                  ([hoeher-als typ2 typ1]
                   (anwenden-generisch op (list (erhoehen arg1) arg2)))))
          (error "Keine Methode für diese Typen -- ANWENDEN-GENERISCH"
                 (list op typen)))))))
