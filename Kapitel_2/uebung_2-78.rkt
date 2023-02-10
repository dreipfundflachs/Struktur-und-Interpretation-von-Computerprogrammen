;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.78 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide etikettieren typ-etikett inhalt
         installieren-scheme-zahl-package
         konstr-scheme-zahl)

; Konstruktoren und Selektoren für Typ-Etiketten:
(define (etikettieren typ-etikett inhalt)
  (if (number? inhalt)
    inhalt
    (cons typ-etikett inhalt)))

(define (typ-etikett datum)
  (cond ([number? datum] 'scheme-zahl)
        ([pair? datum] (car datum))
        (else error "Fehler beim Datentyp -- TYP-ETIKETT" datum)))

(define (inhalt datum)
  (cond ([number? datum] datum)
        ([pair? datum] (cdr datum))
        (else error "Fehler beim Datentyp -- INHALT" datum)))

; Scheme-Zahl-Package:
(define (installieren-scheme-zahl-package)
  (put 'add '(scheme-zahl scheme-zahl) +)
  (put 'sub '(scheme-zahl scheme-zahl) -)
  (put 'mul '(scheme-zahl scheme-zahl) *)
  (put 'div '(scheme-zahl scheme-zahl) /)
  (put 'konstr 'scheme-zahl identity)
  'fertig)

(define (konstr-scheme-zahl n)
  ((get 'konstr 'scheme-zahl) n))
