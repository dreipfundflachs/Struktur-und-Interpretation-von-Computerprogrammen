;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  SICP - Abschnitt 2.2.4  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(define welle2 (neben welle (kippen-vert welle)))

; (define welle4 (unter welle2 welle2))

(define (gekippte-paare maler)
  (let ((maler2 (neben maler (kippen-vert maler))))
    (unter maler2 maler2)))

(define welle4 (gekippte-paare welle))

(define (rechts-geteilt maler n)
  (if (= n 0)
    maler
    (let ((kleiner (rechts-geteilt maler (- n 1))))
      (neben maler (unter kleiner kleiner)))))

(define (eck-geteilt maler n)
  (if (= n 0)
    maler
    (let ([oben (oben-geteilt maler (- n 1))]
          [rechts (rechts-geteilt maler (- n 1))])
      (let ([oben-links (neben oben oben)]
            [unten-rechts (unter rechts rechts)]
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

(define (quadratische-grenzen maler n)
  (let ([viertel (eck-geteilt maler n)])
    (let ([halb (neben (kippen-horiz viertel) viertel)])
      (unter (kippen-vert halb) halb))))

(define (quadrat-aus-vier o-l o-r u-l u-r)
  (lambda (maler)
    (let ([oben (neben (o-l maler) (o-r maler))]
          [unten (neben (u-l maler) (u-r maler))])
      (unter unten oben))))

(define (gekippte-paar maler)
  (let ((kombieniere4 (quadrat-aus-vier identitaet kippen-vert
                                        identitaet kippen-vert)))
    (kombieniere4 maler)))

(define (quadratische-grenzen maler n)
  (let ([kombiniere4 (quadrat-aus-vier kippen-horiz identitaet
                                       drehe180 kippen-vert)])
    (kombiniere4 (eck-geteilt maler n))))

(define (rahmen-koord-abb rahmen)
  (lambda (v)
    (add-vekt
      (ursprung-rahmen rahmen)
      (add-vekt (skaliere-vekt (xkoord-vekt v) (kante1-rahmen rahmen))
                (skaliere-vekt (ykoord-vekt v) (kante2-rahmen rahmen))))))

(define (fuer-jedes proz elemente)
  (cond ((null? elemente) (display ""))
        (else
          (proz (car elemente)) (fuer-jedes proz (cdr elemente)))))

(define (strecken->maler strecken-liste)
  (lambda (rahmen)
    (fuer-jedes
      (lambda (strecke)
        (zeichne-linie
          ((rahmen-koord-abb rahmen) (start-strecke strecke))
          ((rahmen-koord-abb rahmen) (ende-strecke strecke))))
      strecken-liste)))

(define (transform-maler maler ursprung ecke1 ecke2)
  (lambda (rahmen)
    (let ([a (rahmen-koord-abb rahmen)])
      (let ([neuer-ursprung (a ursprung)])
        (maler
          (konstr-rahmen neuer-ursprung
                        (sub-vekt (a ecke1) neuer-ursprung)
                        (sub-vekt (a ecke2) neuer-ursprung)))))))

(define (kippen-vert maler)
  (transform-maler maler
                   (konstr-vekt 0.0 1.0)     ; neuer 'ursprung'
                   (konstr-vekt 1.0 1.0)     ; neuer Endpunkt von 'kante1'
                   (konstr-vekt 0.0 0.0)))   ; neuer Endpunkt von 'kante2'

(define (schrumpfe-nach-oben-rechts maler)
  (transform-maler maler
                   (konstr-vekt 0.5 0.5)     ; neuer 'ursprung'
                   (konstr-vekt 1.0 0.5)     ; neuer Endpunkt von 'kante1'
                   (konstr-vekt 0.5 1.0)))   ; neuer Endpunkt von 'kante2'

(define (drehen90 maler)
  (transform-maler maler
                   (konstr-vekt 0.0  0.0)    ; neuer 'ursprung'
                   (konstr-vekt 0.0  1.0)    ; neuer Endpunkt von 'kante1'
                   (konstr-vekt -1.0 0.0)))  ; neuer Endpunkt von 'kante2'

(define (stauchen maler)
  (transform-maler maler
                   (konstr-vekt 0.0  0.0)    ; neuer 'ursprung'
                   (konstr-vekt 0.65 0.35)   ; neuer Endpunkt von 'kante1'
                   (konstr-vekt 0.35 0.65))) ; neuer Endpunkt von 'kante2'

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
 
(define identitaet identity)
