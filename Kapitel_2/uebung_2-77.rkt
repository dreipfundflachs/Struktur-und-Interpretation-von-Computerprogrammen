;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.77 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

; Der Vorschlag von Alyssa P. Hacker funktioniert, weil durch die Hinzufügung
; dieser vier Zeilen, die entsprechende Selektoren der Rechteck- und
; Polar-Packages zugänglich werden. Die Anwendung von 'abs-wert' auf eine
; komplexe Zahl z streift die Etikett 'komplex' ab und dirigiert den Inhalt
; (z.B., (cons 'rechteck (cons 3 4))) in das Rechteck- oder Polarmodul, wo die
; entsprechende Version von 'abs-wert' noch einmal aufgerufen wird.

; Beispiel: Anwendung auf (cons 'komplex (cons 'rechteck (cons 3 4)))
;
;    (abs-wert z)
; -> (anwenden-generisch 'abs-wert z)
; -> (apply (get 'abs-wert '(komplex)) (list (inhalt z)))
; -> (abs-wert (inhalt z))
; -> (anwenden-generisch 'abs-wert (inhalt z))
; -> (apply (get 'abs-wert '(rechteck)) (list (inhalt (inhalt z))))
; -> (abs-wert (inhalt (inhalt z)))
; -> (abs-wert (cdr (cdr z)))
; -> (abs-wert (cons 3 4))
; -> (sqrt (+ (quadrat (reeller-teil (cons 3 4)))
;             (quadrat (imag-teil (cons 3 4)))))
; -> (sqrt (+ 9 16))
; -> 5

; 'anwenden-generisch' wird zweimal aufgerufen. Zugeteilt wird die Prozedur
; 'abs-wert' des Pakets 'komplex' (bzw. des Pakets 'rechteck') im ersten (bzw.
; im zweiten) Fall.
