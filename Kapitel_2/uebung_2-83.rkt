;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.83 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide erhoehen)

(define (installieren-ganzzahliges-package)
  ;; interne Prozeduren:
  (define (erhoehen n) (konstr-rational n 1))
  ;...
  ;; Schnittstelle zum übrigen System:
  (put 'erhoehen '(ganzzahlig) erhoehen)
  ; ...
  'fertig)

(define (installieren-rationales-package)
  ;; interne Prozeduren:
  (define (erhoehen r)
    (konstr-reell (/ (zaehler r) (nenner r))))
  ; ...
  ;; Schnittstelle zum übrigen System:
  (put 'erhoehen '(rational) erhoehen)
  ; ...
  'fertig)

(define (installieren-reelles-package)
  ;; interne Prozeduren:
  (define (erhoehen t) (konstr-komplex-aus-reell-imag t 0))
  ; ...
  ;; Schnittstelle zum übrigen System:
  (put 'erhoehen '(reell) erhoehen)
  ; ...
  'fertig)

; Generische Operation 'erhoehen':
(define (erhoehen x) (anwenden-generisch 'erhoehen x))
