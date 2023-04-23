;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  SICP - Abschnitt 3.3.3  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(provide suche-satz massoc
         eintragen!
         konstr-tabelle
         suche-satz-2 eintragen-2!)

; Implementierung von eindimensionalen Tabellen:
(define (massoc schluessel saetze)
  (cond [(null? saetze) false]
        [(equal? schluessel (mcar (mcar saetze))) (mcar saetze)]
        [else (massoc schluessel (mcdr saetze))]))

(define (suche-satz schluessel tabelle)
  (let [(satz (massoc schluessel (mcdr tabelle)))]
    (if satz
      (mcdr satz)
      false)))

(define (eintragen! schluessel wert tabelle)
  (let [(satz (massoc schluessel tabelle))]
    (if satz
      (set-cdr! satz wert)
      (set-cdr! tabelle (mcons (mcons schluessel wert)
                               (mcdr tabelle)))))
  'ok!)

(define (konstr-tabelle)
  (mlist "*tabelle*"))

; Implementierung von zweidimensionalen Tabellen:
(define (suche-satz-2 schluessel-1 schluessel-2 tabelle)
  (let [(untertabelle (massoc schluessel-1 (mcdr tabelle)))]
    (if untertabelle
      (let [(satz (massoc schluessel-2 (mcdr untertabelle)))]
        (if satz
          (mcdr satz)
          false))
      false)))

(define (eintragen-2! schluessel-1 schluessel-2 wert tabelle)
  (let [(untertabelle (massoc schluessel-1 tabelle))]
    (if untertabelle
      (let [(satz (massoc schluessel-2 untertabelle))]
        (if satz
          (set-cdr! satz wert)
          (set-cdr! (mcdr untertabelle)
                    (mcons (mcons schluessel-2 wert)
                           (mcdr untertabelle)))))
      (set-cdr! (mcdr tabelle)
                (mcons (list schluessel-1 (mcons schluessel-2 wert))
                (mcdr tabelle))))))


; Implementierung von lokalen Tabellen:

