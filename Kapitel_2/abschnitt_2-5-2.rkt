;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  SICP - Abschnitt 2.5.2  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide add-komplex-mit-scheme-zahl
         scheme-zahl->komplex
         anwenden-generisch)

(define (add-komplex-mit-scheme-zahl z x)
  (konstr-aus-reell-imag (+ (reeller-teil z) x)
                         (imag-teil z)))

(put 'add '(komplex scheme-zahl)
     (lambda z x) (etikett (add-komplex-mit-scheme-zahl z x)))

(define (scheme-zahl->komplex n)
  (konstr-komplex-aus-reell-imag (inhalt n) 0))

(put-typanpassung 'scheme-zahl 'komplex scheme-zahl->komplex)

(define (anwenden-generisch op .args)
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
