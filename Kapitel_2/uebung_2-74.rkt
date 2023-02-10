;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.74 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide etikettieren bereich inhalt
         datensatz-abrufen gehalt-abrufen finde-angestellten)

(define (etikettieren bereich-etikett inhalt)
  (cons bereich-etikett inhalt))

(define (bereich datensatz) (car datensatz))

(define (inhalt datensatz) (cdr datensatz))

; (a) 
(define (datensatz-abrufen id datensatz)
  ((get 'datensatz-abrufen (bereich datensatz)) id datensatz))

; (b)
(define (gehalt-abrufen datensatz)
  ((get 'gehalt-abrufen (bereich datensatz) (inhalt datensatz))))

; (c)
(define (finde-angestellten-daten name dateien)
  (if (null? dateien)
    (error "Angestellte nicht gefunden: FINDE-ANGESTELLTEN-DATEN" name)
    (let ([datei (car dateien)])
      (if ((get 'name-abrufen (bereich datei)) name)
        ((get 'name-abrufen (bereich datei) name))
        (finde-angestellten-daten name (cdr dateien))))))

; (d)
; Man muss die entsprechende Prozeduren in Form eines neuen Pakets für diese
; Firma installieren. Insbesondere müssen in diesem Paket Implementationen der
; Prozeduren 'datensatz-abrufen', 'gehalt-abrufen' und 'finde-angestellten'
; vorhanden sein.
