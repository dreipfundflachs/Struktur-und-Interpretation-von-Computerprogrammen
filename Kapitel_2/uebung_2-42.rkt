;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.42 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide akkumuliere glattabb durchzaehlen-intervall glaetten
         konstr-pos linie reihe
         leeres-brett sicher? hinzufuegen-position
         erweitere-liste damen-linien damen
         pstn1 pstn2 pos1 pos2 pos3 pos4 pos5 pstn-liste)

; Zusätzliche Prozeduren:
(define (akkumuliere op anfangswert sequenz)
  (if (null? sequenz)
    anfangswert
    (akkumuliere op (op anfangswert (car sequenz)) (cdr sequenz))))

(define (glattabb proc seq)
  (akkumuliere append null (map proc seq)))

(define (durchzaehlen-intervall i j)
  (if (> i j)
    null
    (cons i (durchzaehlen-intervall (+ i 1) j))))

(define (glaetten xss) (foldr append null xss))

(define (car1 xs)
  (if (null? xs)
    null
    (car xs)))

; Konstruktoren und Selektoren für Positionen:
(define (konstr-pos m n) (cons m n))

(define (linie pos) (car pos))

(define (reihe pos) (cdr pos))

(define leeres-brett null)

(define (hinzufuegen-position pos positionen) (cons pos positionen))

(define (moegliche-positionen linie brett-groesse)
  (map (lambda (j) (cons linie j)) (durchzaehlen-intervall 1 brett-groesse)))

; Hauptfunktionen:
(define (sicher? pos positionen)
  (let ([erste (car1 positionen)])
    (cond ([null? positionen]
           true)
          ([or (= (linie pos) (linie erste))
               (= (reihe pos) (reihe erste))
               (= (- (reihe pos) (linie pos))
                  (- (reihe erste) (linie erste)))
               (= (+ (reihe pos) (linie pos))
                  (+ (reihe erste) (linie erste)))]
           false)
          (else (sicher? pos (cdr positionen))))))

(define (erweitere-liste pos positionen-liste)
  (filter (compose not null?)
          (map (lambda (positionen)
                 (if (sicher? pos positionen)
                   (hinzufuegen-position pos positionen)
                   null))
               positionen-liste)))

(define (damen-linien i ergebnis brett-groesse)
    (if (= i 0)
      ergebnis
      (damen-linien (- i 1)
                    (glaetten (map (lambda (j) (erweitere-liste (cons i j)
                                                                ergebnis))
                              (durchzaehlen-intervall 1 brett-groesse)))
                    brett-groesse)))

(define (damen brett-groesse) (damen-linien brett-groesse
                                            (list null)
                                            brett-groesse))

; Beispiel:

(define pstn1 (list (cons 1 4)
                    (cons 2 2)
                    (cons 3 8)
                    (cons 4 5)))

(define pstn2 (list (cons 1 1)
                    (cons 2 2)
                    (cons 3 3)
                    (cons 4 4)))

(define pstn-liste (list pstn1 pstn2))

(define pos1 (cons 5 1))

(define pos2 (cons 5 2))

(define pos3 (cons 5 3))

(define pos4 (cons 5 4))

(define pos5 (cons 5 5))

(newline)
(display "Anzahl der möglichen *partiellen* Lösungen")
(display " für alle Brettgrössen zwischen 1 und 12")
(newline)
(display (map length (map (lambda (i) (damen-linien i (list null) 8))
                          (durchzaehlen-intervall 1 12))))
(newline)

(newline)
(display "Anzahl der möglichen Lösungen für alle Brettgrössen zwischen 1 und 12")
(newline)
(display (map (compose length damen) (durchzaehlen-intervall 1 12)))
(newline)
