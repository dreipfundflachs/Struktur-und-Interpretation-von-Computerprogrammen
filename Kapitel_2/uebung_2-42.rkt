;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.42 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide damen durchzaehlen-intervall glaetten)

(define (damen brett-groesse)
  (define (damen-linien k)
    (if (= k 0)
      (list leeres-brett)
      (filter
        (lambda (positionen) (sicher? k positionen))
        (glattabb
          (lambda (rest-der-damen)
            (abb (lambda (neue-reihe)
                   (hinzufuegen-position neue-reihe k
                                         rest-der-damen))
                 (durchzaehlen-intervall 1 brett-groesse)))
          (damen-linien (- k 1))))))
  (damen-linien brett-groesse))

(define (durchzaehlen-intervall i j)
  (if (> i j)
    null
    (cons i (durchzaehlen-intervall (+ i 1) j))))

(define (glaetten xss) (foldr append null xss))
