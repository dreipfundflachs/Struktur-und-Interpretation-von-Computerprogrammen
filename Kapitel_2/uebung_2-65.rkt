;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.65 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide vereinigungs-baum schnitt-baum
         baum->liste liste->baum
         eintrag linker-ast rechter-ast konstr-baum
         baum-1 baum-2 baum-3)

; Im Gegensatz zur Aussage der Übung, um θ(n) Prozeduren zu erhalten, muss man
; annehmen, dass die Mengen nicht nur als ausgewogene binäre Bäume repräsentiert
; sind, sondern als binäre _Suchbäume_. 

(define (vereinigungs-baum baum-1 baum-2)
  (liste->baum (vereinigung (baum->liste baum-1)
                             (baum->liste baum-2))))

(define (schnitt-baum baum-1 baum-2)
  (liste->baum (schnitt-menge (baum->liste baum-1)
                              (baum->liste baum-2))))

(define (vereinigung menge-1 menge-2)
  (cond [(null? menge-1) menge-2]
        [(null? menge-2) menge-1]
        [else (let [(x1 (car menge-1)) (x2 (car menge-2))]
                (cond [(= x1 x2) (cons x1 (vereinigung (cdr menge-1)
                                                       (cdr menge-2)))]
                      [(< x1 x2) (cons x1 (vereinigung (cdr menge-1)
                                                       menge-2))]
                      [(< x2 x1) (cons x2 (vereinigung menge-1
                                                       (cdr menge-2)))]))]))
(define (schnitt-menge menge-1 menge-2)
  (if (or (null? menge-1) (null? menge-2))
    null
    (let [(x1 (car menge-1))
          (x2 (car menge-2))]
      (cond [(= x1 x2) (cons x1 (schnitt-menge (cdr menge-1)
                                                         (cdr menge-2)))]
            [(< x1 x2) (schnitt-menge (cdr menge-1) menge-2)]
            [(> x1 x2) (schnitt-menge menge-1 (cdr menge-2))]))))

(define (baum->liste baum)
  (define (kopiere-in-liste baum ergebnis-liste)
    (if (null? baum)
      ergebnis-liste
      (kopiere-in-liste (linker-ast baum)
                        (cons (eintrag baum)
                              (kopiere-in-liste (rechter-ast baum)
                                                ergebnis-liste)))))
  (kopiere-in-liste baum null))

(define (eintrag baum) (car baum))

(define (linker-ast baum) (cadr baum))

(define (rechter-ast baum) (caddr baum))

(define (konstr-baum eintrag links rechts)
  (list eintrag links rechts))

(define (liste->baum elemente)
  (car (teil-baum elemente (length elemente))))

(define (teil-baum elte n)
  (if (= n 0)
    (cons null elte)
    (let ([linke-groesse (quotient (- n 1) 2)])
      (let ([linkes-ergebnis (teil-baum elte
                                        linke-groesse)])
        (let ([linker-baum (car linkes-ergebnis)]
              [nicht-linke-elte (cdr linkes-ergebnis)]
              [rechte-groesse (- n (+ linke-groesse 1))])
          (let ([dieser-eintrag (car nicht-linke-elte)]
                [rechtes-ergebnis (teil-baum
                                    (cdr nicht-linke-elte)
                                    rechte-groesse)])
            (let ([rechter-baum (car rechtes-ergebnis)]
                  [restliche-elte (cdr rechtes-ergebnis)])
              (cons (konstr-baum dieser-eintrag
                                 linker-baum
                                 rechter-baum)
                    restliche-elte))))))))

; Beispiele:

(define baum-1 (list 7 (list 3 (list 1 null null) (list 5 null null))
                     (list 9 null (list 11 null null))))

(define baum-2 (list 3 (list 1 null null)
                     (list 7 (list 5 null null)
                           (list 9 null (list 11 null null)))))

(define baum-3 (list 5 (list 3 (list 1 null null) null)
                     (list 9 (list 7 null null) (list 11 null null))))
