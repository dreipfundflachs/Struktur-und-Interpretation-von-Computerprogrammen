;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.03 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide konstr-punkt x-koord y-koord gleiche-punkte?
         konstr-rechteck diag-punkt-1 diag-punkt-2 anti-diag-1 anti-diag-2
         gleiche-re? drucke-rechteck flaeche umfang
         br ho konstr-re ecke breite hoehe
         diag-1 diag-2 anti-1 anti-2
         p1 q1 r1 p2 q2 r2)

; Implementierung von Vektoren:

(define (konstr-punkt x y) (cons x y))

(define (x-koord p) (car p))

(define (y-koord p) (cdr p))

(define (gleiche-punkte? p q)
  (and (= (x-koord p) (x-koord q))
       (= (y-koord p) (y-koord q))))

(define (drucke-punkt p)
  (display "(")
  (display (x-koord p))
  (display " , ")
  (display (y-koord p))
  (display ")"))

; Implementierung von Rechtecken als ein Paar von Punkten, die durch eine
; Diagonale verbunden sind:

(define (konstr-rechteck ecke breite hoehe) (list ecke breite hoehe))

(define (diag-punkt-1 rechteck) (car rechteck))

(define (diag-punkt-2 rechteck) (cdr rechteck))

(define (br rechteck)
  (let ((p (diag-punkt-1 rechteck))
        (q (diag-punkt-2 rechteck)))
    (abs (- (x-koord p) (x-koord q)))))

(define (ho rechteck)
  (let ((p (diag-punkt-1 rechteck))
        (q (diag-punkt-2 rechteck)))
    (abs (- (y-koord p) (y-koord q)))))

(define (linke-untere-ecke rechteck)
  (let ((p (diag-punkt-1 rechteck))
        (q (diag-punkt-2 rechteck)))
    (let ((x (min (x-koord p) (x-koord q)))
          (y (min (y-koord p) (y-koord q))))
      (konstr-punkt x y))))

(define (anti-diag-1 rechteck)
  (konstr-punkt (x-koord (diag-punkt-1 rechteck))
                (y-koord (diag-punkt-2 rechteck))))

(define (anti-diag-2 rechteck)
  (konstr-punkt (x-koord (diag-punkt-2 rechteck))
                (y-koord (diag-punkt-1 rechteck))))

; Implementierung von Rechtecken als eine Liste, die aus ihrer linken unteren
; Ecke, Breite und Höhe besteht:
;  |
;  |
;  . ----

(define (konstr-re ecke breite hoehe) (list ecke breite hoehe))

(define (ecke rechteck) (car rechteck))

(define (breite rechteck) (cadr rechteck))

(define (hoehe rechteck) (caddr rechteck))

(define (diag-1 rechteck) (ecke rechteck))

(define (diag-2 rechteck)
  (let ((x (+ (x-koord (ecke rechteck)) (breite rechteck)))
        (y (+ (y-koord (ecke rechteck)) (hoehe rechteck))))
    (konstr-punkt x y)))

(define (anti-1 rechteck)
  (let ((x (x-koord (ecke rechteck)))
        (y (+ (y-koord (ecke rechteck)) (hoehe rechteck))))
    (konstr-punkt x y)))

(define (anti-2 rechteck)
  (let ((x (+ (x-koord (ecke rechteck)) (breite rechteck)))
        (y (y-koord (ecke rechteck))))
    (konstr-punkt x y)))

; Einschlägige Prozeduren:

(define (flaeche r)
  (* (breite r) (hoehe r)))

(define (umfang r)
  (* 2 (+ (breite r) (hoehe r))))

(define (gleiche-re? re-1 re-2)
  (= (re-1) (re-2)))

(define (drucke-rechteck r)
  (drucke-punkt (anti-1 r))
  (display " -------- ")
  (drucke-punkt (diag-2 r))
  (newline)
  (display "   |                |  ")
  (newline)
  (display "   |                |  ")
  (newline)
  (display "   |                |  ")
  (newline)
  (drucke-punkt (diag-1 r))
  (display " -------- ")
  (drucke-punkt (anti-2 r))
  (newline))

; Beispiele von Rechtecken:

(define p1 (konstr-punkt 0 1))

(define q1 (konstr-punkt 1 0))

(define p2 (konstr-punkt 0 0))

(define q2 (konstr-punkt 2 3))

(define r1 (konstr-re p2 2 3))

(define r2 (konstr-re p1 1 1))
