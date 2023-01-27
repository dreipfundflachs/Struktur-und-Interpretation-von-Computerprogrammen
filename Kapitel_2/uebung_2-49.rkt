;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.49 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide maler-rahmen maler-X maler-Karo welle)


(define maler-rahmen
  (let ([uli (konstr-vekt 0 0)]
        [ure (konstr-vekt 1 0)]
        [oli (konstr-vekt 0 1)]
        [ore (konstr-vekt 1 1)])
    (let ([strecken-liste
            (list (konstr-strecke uli ure)
                  (konstr-strecke ure ore)
                  (konstr-strecke ore oli)
                  (konstr-strecke oli uli))])
      (strecken->maler strecken-liste))))

(define maler-X
  (let ([uli (konstr-vekt 0 0)]
        [ure (konstr-vekt 1 0)]
        [oli (konstr-vekt 0 1)]
        [ore (konstr-vekt 1 1)])
    (let ([strecken-liste
            (list (konstr-strecke uli ore)
                  (konstr-strecke oli ure))])
      (strecken->maler strecken-liste))))

(define maler-Karo
  (let ([oben (konstr-vekt   0.5 1)]
        [links (konstr-vekt  0   0.5)]
        [rechts (konstr-vekt 1   0.5)]
        [unten (konstr-vekt  0.5 0)])
    (let ([strecken-liste
            (list (konstr-strecke unten rechts)
                  (konstr-strecke rechts oben)
                  (konstr-strecke oben links)
                  (konstr-strecke links unten))])
      (strecken->maler strecken-liste))))

(define welle
  (strecken->maler welle-strecken-liste))

(define welle-strecken-liste
  (list
    (konstr-strecke (konstr-vekt .25 0) (konstr-vekt .35 .5))
    (konstr-strecke (konstr-vekt .35 .5) (konstr-vekt .3 .6))
    (konstr-strecke (konstr-vekt .3 .6) (konstr-vekt .15 .4))
    (konstr-strecke (konstr-vekt .15 .4) (konstr-vekt 0 .65))
    (konstr-strecke (konstr-vekt 0 .65) (konstr-vekt 0 .85))
    (konstr-strecke (konstr-vekt 0 .85) (konstr-vekt .15 .6))
    (konstr-strecke (konstr-vekt .15 .6) (konstr-vekt .3 .65))
    (konstr-strecke (konstr-vekt .3 .65) (konstr-vekt .4 .65)) 
    (konstr-strecke (konstr-vekt .4 .65) (konstr-vekt .35 .85)) 
    (konstr-strecke (konstr-vekt .35 .85) (konstr-vekt .4 1)) 
    (konstr-strecke (konstr-vekt .4 1) (konstr-vekt .6 1)) 
    (konstr-strecke (konstr-vekt .6 1) (konstr-vekt .65 .85)) 
    (konstr-strecke (konstr-vekt .65 .85) (konstr-vekt .6 .65)) 
    (konstr-strecke (konstr-vekt .6 .65) (konstr-vekt .75 .65)) 
    (konstr-strecke (konstr-vekt .75 .65) (konstr-vekt 1 .35)) 
    (konstr-strecke (konstr-vekt 1 .35) (konstr-vekt 1 .15)) 
    (konstr-strecke (konstr-vekt 1 .15) (konstr-vekt .6 .45)) 
    (konstr-strecke (konstr-vekt .6 .45) (konstr-vekt .75 0)) 
    (konstr-strecke (konstr-vekt .75 0) (konstr-vekt .6 0)) 
    (konstr-strecke (konstr-vekt .6 0) (konstr-vekt .5 .3)) 
    (konstr-strecke (konstr-vekt .5 .3) (konstr-vekt .4 0)) 
    (konstr-strecke (konstr-vekt .4 0) (konstr-vekt .25 0))))

(define (strecken->maler strecken-liste)
  (lambda (rahmen)
    (fuer-jedes
      (lambda (strecke)
        (zeichne-linie
          ((rahmen-koord-abb rahmen) (start-strecke strecke))
          ((rahmen-koord-abb rahmen) (ende-strecke strecke))))
      strecken-liste)))

(define (fuer-jedes proz elemente)
  (cond ((null? elemente) (display ""))
        (else
          (proz (car elemente)) (fuer-jedes proz (cdr elemente)))))

; Implementation von Strecken als Paare von Vektoren:

(define (konstr-strecke anfangspunkt endpunkt)
  (cons anfangspunkt endpunkt))

(define (start-strecke strecke) (car strecke))

(define (ende-strecke strecke) (cdr strecke))

; Implementation von Vektoren als Paare von Koordinaten:

(define (konstr-vekt x y) (cons x y))

(define (xkoord-vekt v) (car v))

(define (ykoord-vekt v) (cdr v))
