;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.50 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide unter unter1)

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

(define (unter1 maler1 maler2)
  (let ([gedrehter-maler1 (drehen270 maler1)]
        [gedrehter-maler2 (drehen270 maler2)])
    (drehen90 (neben gedrehter-maler1 gedrehter-maler2))))

(define (drehen90 maler)
  (transform-maler maler
                   (konstr-vekt 0.0  0.0)    ; neuer 'ursprung'
                   (konstr-vekt 0.0  1.0)    ; neuer Endpunkt von 'kante1'
                   (konstr-vekt -1.0 0.0)))  ; neuer Endpunkt von 'kante2'

(define (drehen270 maler)
  (transform-maler maler
                   (konstr-vekt  0.0   0.0)     ; neuer 'ursprung'
                   (konstr-vekt  0.0  -1.0)     ; neuer Endpunkt von 'kante1'
                   (konstr-vekt  1.0   0.0)))   ; neuer Endpunkt von 'kante2'

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

; Implementationen von Vektoren und Rahmen:

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

