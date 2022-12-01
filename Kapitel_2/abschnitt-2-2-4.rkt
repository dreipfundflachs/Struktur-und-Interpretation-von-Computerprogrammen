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
    (let ((oben (oben-geteilt maler (- n 1)))
          (rechts (rechts-geteilt maler (- n 1))))
      (let ((oben-links (neben oben oben))
            (unten-rechts (unter rechts rechts))
            (ecke (eck-geteilt maler (- n 1))))
        (neben (unter maler oben-links)
               (unter rechts ecke))))))

(define (quadratische-grenzen maler n)
  (let ((viertel (eck-geteilt maler n)))
    (let (halb (neben (kippen-horiz viertel)
                      viertel))
      (unter (kippen-vert halb) halb))))

(define (quadrat-aus-vier o-l o-r u-l u-r)
  (lambda (maler)
    (let ((oben (neben (o-l maler) (o-r maler)))
          (unten (neben (u-l maler) (u-r maler))))
      (unter unten oben))))

(define (gekippte-paar maler)
  (let ((kombieniere4 (quadrat-aus-vier identitaet kippen-vert
                                        identitaet kippen-vert)))
    (kombieniere4 maler)))

(define (quadratische-grenzen maler n)
  (let ((kombiniere4
          (quadrat-aus-vier kippen-horiz identitaet
                            drehe180 kippen-vert)))
    (kombiniere4 (eck-geteilt maler n))))

(define (rahmen-koord-abb rahmen)
  (lambda (v)
    (add-vekt
      (ursprung-rahmen rahmen)
      (add-vekt (skaliere-vekt (xkoord-vekt v) (kante1-rahmen rahmen))
                (skaliere-vekt (ykoord-vekt v) (kante2-rahmen rahmen))))))
