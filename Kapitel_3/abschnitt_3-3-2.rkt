;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  SICP - Abschnitt 3.3.2  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(provide konstr-warteschlange
         leere-warteschlange? anfang-warteschlange
         anfangs-zgr ende-zgr
         set-anfangs-zgr! set-ende-zgr!
         hinzufuegen-warteschlange! entfernen-warteschlange!
         q)

(define (konstr-warteschlange) (mcons '() '()))

(define (leere-warteschlange? warteschlange)
  (null? (anfangs-zgr warteschlange)))

(define (anfangs-zgr warteschlange) (mcar warteschlange))

(define (ende-zgr warteschlange) (mcdr warteschlange))

(define (set-anfangs-zgr! warteschlange element)
  (set-car! warteschlange element))

(define (set-ende-zgr! warteschlange element)
  (set-cdr! warteschlange element))

(define (anfang-warteschlange warteschlange)
  (if (leere-warteschlange? warteschlange)
    (error "ANFANG mit leerer Warteschlange aufgerufen" warteschlange)
    (mcar (anfangs-zgr warteschlange))))

(define (hinzufuegen-warteschlange! warteschlange element)
  (let [(neues-paar (mcons element '()))]
    (cond ([leere-warteschlange? warteschlange]
           (set-anfangs-zgr! warteschlange neues-paar)
           (set-ende-zgr! warteschlange neues-paar)
           warteschlange)
          (else
            (set-cdr! (ende-zgr warteschlange) neues-paar)
            (set-ende-zgr! warteschlange neues-paar)
            warteschlange))))

(define (entfernen-warteschlange! warteschlange)
  (cond ([leere-warteschlange? warteschlange]
         (error "ENTFERNEN mit leerer Warteschlange aufgerufen"
                warteschlange))
        (else
          (set-anfangs-zgr! warteschlange
                            (mcdr (anfangs-zgr warteschlange)))
          warteschlange)))


(define q (konstr-warteschlange))

(hinzufuegen-warteschlange! q 'a)
(display q)
(newline)

(hinzufuegen-warteschlange! q 'b)
(display q)
(newline)

(entfernen-warteschlange! q)
(display q)
(newline)

(hinzufuegen-warteschlange! q 'c)
(display q)
(newline)

(hinzufuegen-warteschlange! q 'd)
(display q)
(newline)

(entfernen-warteschlange! q)
(display q)
(newline)
