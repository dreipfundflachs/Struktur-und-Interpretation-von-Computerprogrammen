;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.82 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(define (anpassen-an typ1)
  (lambda (typ) (get-typanpassung typ1 typ)))

(define (all praedikate)
  (cond ([null? praedikate] #t)
        ([car praedikate] (all (cdr praedikate)))
        (else #f)))

(define (multi-map abbildungen elemente)
  (let ([abbildung1 (car abbildungen)]
        [element1 (car elemente)]
        [uebrige-abbildungen (cdr abbildungen)]
        [uebrige-elemente (cdr elemente)])
    (cons (abbildung1 element1)
          (multi-map uebrige-abbildungen uebrige-elemente))))

(define (anwenden-generisch op . args)
  (define (iter op typ-etiketten neue-typen . args)
  (let ([typ-etiketten (map typ-etikett args)])
      (define (iter uebrige-typen )
        (let ([typ1 (car uebrige-typen)]
              [angepasste-typen (map (anpassen-an typ1) typ-etiketten)])
          (if (and (all angepasste-typen)
                   ()
            (let ([angepasste-args (multi-map angepasste-typen args)]))
            )
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
