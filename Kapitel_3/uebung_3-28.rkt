;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 3.28 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

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

(define (logisches-oder s t)
  (cond [(= s 0) t]
        [(= s 1) 1]
        [else (error "Ungültige Signale: LOGISCHES-ODER")]))
