;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.81 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(define (anwenden-generisch op . args)
  (let ([typ-etiketten (map typ-etikett args)])
    (let ([proc (get op typetiketten)])
      (if proc
        (apply proc (map inhalt args))
        (if (= (length args) 2)
          (let ([typ1 (car typ-etiketten)]
                [typ2 (cadr typ-etiketten)]
                [a1 (car args)]
                [a2 (cadr args)])
            (let ([t1->t2 (get-typanpassung typ1 typ2)]
                  [t2->t1 (get-typanpassung typ2 typ1)])
              (cond (t1->t2 (anwenden-generisch op (t1->t2 a1) a2))
                    (t2->t1 (anwenden-generisch op a1 (t2->t1 a2)))
                    (else error "Keine Methode für diese Typen"
                          (list op typ-etiketten)))))
          (error "Keine Methode für diese Typen"
                 (list op typ-etiketten)))))))

(define (pot x y) (anwenden-generisch 'pot x y))

;; folgendes in das Schemezahlpackage aufgenommen:
(put 'pot '(scheme-zahl scheme-zahl)
     (lambda (x y) (etikett (expt x y))))

; (a) und (b):  Nehmen wir an, wir versuchten 'anwenden-generisch' auf zwei
; Argumente desselben Typs T und auf eine Operation anzuwenden, die für
; Argumente dieses Typs nicht implementiert ist. Dann versucht
; 'anwenden-generisch' zunächst das erste Argument an den Typ T ,,anzupassen''
; und dann das zweite Argument. Wenn wir mit der ursprünglichen
; Definition von 'anwenden-generisch' zu tun haben, dann erhalten wir einen
; Fehler, weil diese Anpassungsprozedur T->T nicht definiert wurde.
;
; Wenn wir aber den Vorschlag von Louis Reasoner folgen würden, dann würde die
; erste Anpassung erfolgreich sein, das erste Argument würde unverändert als
; Ergebnis geliefert werden, und endlich würde 'anwenden-generisch' noch einmal
; aufgerufen werden, und so weiter unaufhörlich.
;
; Deshalb funktioniert 'anwenden-generisch' korrekt so wie es
; ursprünglich implementiert wurde. Dagegen ist der Vorschlag von Louis
; Reasoner eine schlechte Idee, weil man sich in einer Endlosschleife gelingt.

; (c)

(define (anwenden-generisch op . args)
  (define (keine-methode typen)
    (error "Keine Methode für diese Typen"
           (list op typen)))
  (let ([typ-etiketten (map typ-etikett args)])
    (let ([proc (get op typetiketten)])
      (if proc
        (apply proc (map inhalt args))
        (if (= (length args) 2)
          (let ([typ1 (car typ-etiketten)]
                [typ2 (cadr typ-etiketten)]
                [a1 (car args)]
                [a2 (cadr args)])
            (if (eq? t1 t2)
              (keine-methode typ-etiketten)
              (let ([t1->t2 (get-typanpassung typ1 typ2)]
                    [t2->t1 (get-typanpassung typ2 typ1)])
                (cond (t1->t2 (anwenden-generisch op (t1->t2 a1) a2))
                      (t2->t1 (anwenden-generisch op a1 (t2->t1 a2)))
                      (else keine-methode typ-etiketten)))))
            (keine-methode typ-etiketten))))))
