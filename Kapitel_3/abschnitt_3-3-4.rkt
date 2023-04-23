;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  SICP - Abschnitt 3.3.4  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(provide konstr-draht
         halbaddierer volladdierer
         get-signal set-signal! add-vorgang!
         verzoegert
         inverter und-gater oder-gatter
         logisches-nicht logisches-und logisches-oder)


(define a (konstr-draht))
(define b (konstr-draht))
(define c (konstr-draht))
(define d (konstr-draht))
(define e (konstr-draht))
(define s (konstr-draht))

(define (halbaddierer a b s c)
  (let [(d (konstr-draht))
        (e (konstr-draht))]
    (oder-gatter a b d)
    (und-gatter a b c)
    (inverter c e)
    (und-gatter d e s)
    'ok!))

(define (volladdierer a b c-in sum c-out)
  (let [(s (konstr-draht))
        (c1 (konstr-draht))
        (c2 (konstr-draht))]
    (halbaddierer b c-in s c1)
    (halbaddierer a s sum c2)
    (oder-gatter c1 c2 c-out)
    'ok!))

(define (inverter eingabe ausgabe)
  (define (invert-eingabe)
    (let [(neuer-wert (logisches-nicht (get-signal eingabe)))]
      (verzoegert inverter-verzoegerung
                  (lambda ()
                    (set-signal! ausgabe neuer-wert)))))
  (add-vorgang! eingabe invert-eingabe)
  'ok!)

(define (und-gatter a1 a2 ausgabe)
  (define (und-vorgang-prozedur)
    (let [(neuer-wert (logisches-und (get-signal a1)
                                     (get-signal a2)))]
      (verzoegert und-gatter-verzoegerung
                  (lambda ()
                    (set-signal! ausgabe neuer-wert)))))
  (add-vorgang! a1 und-vorgang-prozedur)
  (add-vorgang! a2 und-vorgang-prozedur)
  'ok!)

(define (oder-gatter o1 o2 ausgabe)
  (define (oder-vorgang-prozedur)
    (let [(neuer-wert (logisches-oder (get-signal o1)
                                      (get-signal o2)))]
      (verzoegert oder-gatter-verzoegerung
                  (lambda ()
                    (set-signal! ausgabe neuer-wert)))))
  (add-vorgang! o1 oder-vorgang-prozedur)
  (add-vorgang! o2 oder-vorgang-prozedur)
  'ok!)

(define (logisches-nicht s)
  (cond [(= s 0) 1]
        [(= s 1) 0]
        [else (error "Ungültiges Signal: LOGISCHES-NICHT" s)]))

(define (logisches-und s t)
  (cond [(= s 0) 0]
        [(= s 1) t]
        [else (error "Ungültige Signale: LOGISCHES-UND")]))

(define (logisches-oder s t)
  (cond [(= s 0) t]
        [(= s 1) 1]
        [else (error "Ungültige Signale: LOGISCHES-ODER")]))
