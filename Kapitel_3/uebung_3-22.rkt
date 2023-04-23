;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 3.22 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(provide konstr-warteschlange
         leere-warteschlange? anfang-warteschlange
         anfangs-zgr end-zgr
         set-anfangs-zgr! set-end-zgr!
         hinzufuegen-warteschlange! entfernen-warteschlange!
         warteschlange-drucken)

(define (konstr-warteschlange)
  (let [(anfangs-zgr '())
        (end-zgr '())]
    (define (leere-warteschlange?)
      (null? anfangs-zgr))
    (define (set-anfangs-zgr! element)
      (set! anfangs-zgr element))
    (define (set-end-zgr! element)
      (set! end-zgr element))
    (define (anfang)
      (if (not (leere-warteschlange?))
          (mcar anfangs-zgr)
          (error "ANFANG mit leerer Warteschlange aufgerufen")))
    (define (drucken)
      (display anfangs-zgr)
      (newline))
    (define (hinzufuegen! element)
      (let [(neues-paar (mcons element '()))]
        (cond [(leere-warteschlange?)
               (set-anfangs-zgr! neues-paar)
               (set-end-zgr! neues-paar)
               (drucken)
               'ok!]
              (else
                (set-cdr! end-zgr neues-paar)
                (set-end-zgr! neues-paar)
                (drucken)
                'ok!))))
    (define (entfernen!)
      (cond [(leere-warteschlange?)
             (error "ENTFERNEN mit leerer Warteschlange aufgerufen")]
            [else
              (set-anfangs-zgr! (mcdr anfangs-zgr))
              (drucken)
              'ok!]))
    (define (zuteilen n)
      (cond [(eq? n 'anfangs-zgr) anfangs-zgr]
            [(eq? n 'end-zgr) end-zgr]
            [(eq? n 'set-anfangs-zgr!) set-anfangs-zgr!]
            [(eq? n 'set-end-zgr!) set-end-zgr!]
            [(eq? n 'hinzufuegen-warteschlange!) hinzufuegen!]
            [(eq? n 'entfernen-warteschlange!) (entfernen!)]
            [(eq? n 'anfang-warteschlange) (anfang)]
            [(eq? n 'leere-warteschlange?) leere-warteschlange?]
            [(eq? n 'drucken) drucken]
            ))
    zuteilen))

(define (leere-warteschlange? warteschlange)
  (warteschlange 'leere-warteschlange?))

(define (anfang-warteschlange warteschlange)
  (warteschlange 'anfang-warteschlange))

(define (anfangs-zgr warteschlange)
  (warteschlange 'anfangs-zgr))

(define (end-zgr warteschlange)
  (warteschlange 'end-zgr))

(define (set-anfangs-zgr! warteschlange element)
  ((warteschlange 'set-anfangs-zgr!) element))

(define (set-end-zgr! warteschlange element)
  ((warteschlange 'set-end-zgr!) element))

(define (warteschlange-drucken warteschlange)
  (warteschlange 'drucken))

(define (hinzufuegen-warteschlange! warteschlange element)
  ((warteschlange 'hinzufuegen-warteschlange!) element))

(define (entfernen-warteschlange! warteschlange)
  (warteschlange 'entfernen-warteschlange!))

(define q1 (konstr-warteschlange))

(hinzufuegen-warteschlange! q1 'a)
(warteschlange-drucken q1)

(hinzufuegen-warteschlange! q1 'b)
(warteschlange-drucken q1)

(entfernen-warteschlange! q1)
(warteschlange-drucken q1)

(entfernen-warteschlange! q1)
(warteschlange-drucken q1)
