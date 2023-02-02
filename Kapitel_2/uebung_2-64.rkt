;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.64 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide liste->baum teil-baum)

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

(define (konstr-baum eintrag links rechts)
  (list eintrag links rechts))
