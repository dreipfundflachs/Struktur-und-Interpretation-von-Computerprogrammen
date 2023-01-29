;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.52 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide welle eck-geteilt quadratische-grenzen)

; (a)

(define welle
  (strecken->maler welle-strecken-liste))

(define welle-strecken-liste
  (list
    (konstr-strecke (konstr-vekt .44 0.7) (konstr-vekt .51 .7))
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


; (b)

(define (eck-geteilt maler n)
  (if (= n 0)
    maler
    (let ([oben (oben-geteilt maler n)]
          [rechts (rechts-geteilt maler n)])
      (let ([oben-links oben]
            [unten-rechts rechts]
            [ecke (eck-geteilt maler (- n 1))])
        (neben (unter maler oben-links)
               (unter rechts ecke))))))

(define (oben-geteilt maler n)
  (if (= n 0)
    maler
    (let ([kleiner (oben-geteilt maler (- n 1))])
      (unter maler (neben kleiner kleiner)))))

(define (rechts-geteilt maler n)
  (if (= n 0)
    maler
    (let ([kleiner (rechts-geteilt maler (- n 1))])
      (neben maler (unter kleiner kleiner)))))

; (c)


(define (quadratische-grenzen maler n)
  (let ([kombiniere4 (quadrat-aus-vier identitaet kippen-horiz
                                       kippen-vert drehen180)])
    (kombiniere4 (eck-geteilt maler n))))

(define (quadrat-aus-vier o-l o-r u-l u-r)
  (lambda (maler)
    (let ([oben (neben (o-l maler) (o-r maler))]
          [unten (neben (u-l maler) (u-r maler))])
      (unter unten oben))))

; Implementationen von Rahmen:

(define (konstr-rahmen ursprung kante1 kante2)
  (list ursprung kante1 kante2))

(define (ursprung-rahmen rahmen)
  (car rahmen))

(define (kante1-rahmen rahmen)
  (cadr rahmen))

(define (kante2-rahmen rahmen)
  (caddr rahmen))

(define (transform-maler maler ursprung ecke1 ecke2)
  (lambda (rahmen)
    (let ([a (rahmen-koord-abb rahmen)])
      (let ([neuer-ursprung (a ursprung)])
        (maler
          (konstr-rahmen neuer-ursprung
                        (sub-vekt (a ecke1) neuer-ursprung)
                        (sub-vekt (a ecke2) neuer-ursprung)))))))

(define (rahmen-koord-abb rahmen)
  (lambda (v)
    (add-vekt
      (ursprung-rahmen rahmen)
      (add-vekt (skaliere-vekt (xkoord-vekt v) (kante1-rahmen rahmen))
                (skaliere-vekt (ykoord-vekt v) (kante2-rahmen rahmen))))))

(define (strecken->maler strecken-liste)
  (lambda (rahmen)
    (fuer-jedes
      (lambda (strecke)
        (zeichne-linie
          ((rahmen-koord-abb rahmen) (start-strecke strecke))
          ((rahmen-koord-abb rahmen) (ende-strecke strecke))))
      strecken-liste)))

(define (konstr-strecke anfangspunkt endpunkt)
  (cons anfangspunkt endpunkt))

(define (start-strecke strecke) (car strecke))

(define (ende-strecke strecke) (cdr strecke))

(define (neben maler1 maler2)
  (let ([geteilt-punkt (konstr-vekt 0.5 0.0)])
    (let ([male-links
            (transform-maler maler1
                             (konstr-vekt 0.0 0.0)
                             geteilt-punkt
                             (konstr-vekt 0.0 1.0))]
          [male-rechts
            (transform-maler maler2
                             geteilt-punkt
                             (konstr-vekt 1.0 0.0)
                             (konstr-vekt 0.5 1.0))])
          (lambda (rahmen)
            (male-links rahmen)
            (male-rechts rahmen)))))

(define (unter maler1 maler2)
  (let ([geteilt-punkt (konstr-vekt 0.0 0.5)])
    (let ([male-unten
            (transform-maler maler1
                             (konstr-vekt 0.0 0.0)
                             (konstr-vekt 1.0 0.0)
                             geteilt-punkt)]
          [male-oben
            (transform-maler maler2
                             geteilt-punkt
                             (konstr-vekt 1.0 0.5)
                             (konstr-vekt 0.0 1.0))])
      (lambda (rahmen)
        (male-unten rahmen)
        (male-oben rahmen)))))

(define (kippen-horiz maler)
  (transform-maler maler
                   (konstr-vekt  0.0   0.0)     ; neuer 'ursprung'
                   (konstr-vekt -1.0   0.0)     ; neuer Endpunkt von 'kante1'
                   (konstr-vekt  0.0   1.0)))   ; neuer Endpunkt von 'kante2'

(define (kippen-vert maler)
  (transform-maler maler
                   (konstr-vekt 0.0 1.0)     ; neuer 'ursprung'
                   (konstr-vekt 1.0 1.0)     ; neuer Endpunkt von 'kante1'
                   (konstr-vekt 0.0 0.0)))   ; neuer Endpunkt von 'kante2'

(define identitaet identity)

(define (drehen180 maler)
  (transform-maler maler
                   (konstr-vekt  0.0   0.0)     ; neuer 'ursprung'
                   (konstr-vekt -1.0   0.0)     ; neuer Endpunkt von 'kante1'
                   (konstr-vekt  0.0  -1.0)))   ; neuer Endpunkt von 'kante2'

(define (fuer-jedes proz elemente)
  (cond ((null? elemente) (display ""))
        (else
          (proz (car elemente)) (fuer-jedes proz (cdr elemente)))))

; Implementation von Vektoren:

(define (konstr-vekt x y) (cons x y))

(define (xkoord-vekt v) (car v))

(define (ykoord-vekt v) (cdr v))

(define (add-vekt u v)
  (konstr-vekt
    (+ (xkoord-vekt u) (xkoord-vekt v))
    (+ (ykoord-vekt u) (ykoord-vekt v))))

(define (sub-vekt u v)
  (konstr-vekt
    (- (xkoord-vekt u) (xkoord-vekt v))
    (- (ykoord-vekt u) (ykoord-vekt v))))

(define (skaliere-vekt c v)
  (konstr-vekt
    (* c (xkoord-vekt v))
    (* c (ykoord-vekt v))))

(define (drucke-vekt v)
  (display "(")
  (display (xkoord-vekt v))
  (display " , ")
  (display (ykoord-vekt v))
  (display ")"))

