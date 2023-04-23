;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 3.21 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(provide konstr-warteschlange
         leere-warteschlange? anfang-warteschlange
         anfangs-zgr end-zgr
         set-anfangs-zgr! set-end-zgr!
         hinzufuegen-warteschlange! entfernen-warteschlange!
         q1
         warteschlange-drucken)

; Erläuterung:
; Diese Ergebnissse werden ausgedruckt, weil wir eine Warteschlange als ein
; Zeigerpaar dargestellt haben. Der erste Zeiger zeigt auf eine Liste, der
; andere auf das letzte Paar dieser Liste. Letzterer Zeiger wird immer
; angezeigt, wenn wir z.B. "display" aufrufen.

; Man bemerke auch, dass die Prozedur `entferne-warteschlange!` nur den ersten
; Zeiger ändert. Deshalb lautet das letzte Ergebnis "{() b}" und nicht einfach 
; "{()}".

(define (konstr-warteschlange) (mcons '() '()))

(define (leere-warteschlange? warteschlange)
  (null? (anfangs-zgr warteschlange)))

(define (anfangs-zgr warteschlange) (mcar warteschlange))

(define (end-zgr warteschlange) (mcdr warteschlange))

(define (set-anfangs-zgr! warteschlange element)
  (set-car! warteschlange element))

(define (set-end-zgr! warteschlange element)
  (set-cdr! warteschlange element))

(define (anfang-warteschlange warteschlange)
  (if (leere-warteschlange? warteschlange)
    (error "ANFANG mit leerer Warteschlange aufgerufen" warteschlange)
    (mcar (anfangs-zgr warteschlange))))

(define (hinzufuegen-warteschlange! warteschlange element)
  (let [(neues-paar (mcons element '()))]
    (cond ([leere-warteschlange? warteschlange]
           (set-anfangs-zgr! warteschlange neues-paar)
           (set-end-zgr! warteschlange neues-paar)
           warteschlange)
          (else
            (set-cdr! (end-zgr warteschlange) neues-paar)
            (set-end-zgr! warteschlange neues-paar)
            warteschlange))))

(define (entfernen-warteschlange! warteschlange)
  (cond ([leere-warteschlange? warteschlange]
         (error "ENTFERNEN mit leerer Warteschlange aufgerufen"
                warteschlange))
        (else
          (set-anfangs-zgr! warteschlange
                            (mcdr (anfangs-zgr warteschlange)))
          warteschlange)))

(define (warteschlange-drucken warteschlange)
  (display (anfangs-zgr warteschlange))
  (newline))

(define q1 (konstr-warteschlange))

(hinzufuegen-warteschlange! q1 'a)
(warteschlange-drucken q1)

(hinzufuegen-warteschlange! q1 'b)
(warteschlange-drucken q1)

(entfernen-warteschlange! q1)
(warteschlange-drucken q1)

(entfernen-warteschlange! q1)
(warteschlange-drucken q1)
