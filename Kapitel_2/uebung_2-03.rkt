;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.03 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Implementierung von Punkten:

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

; Implementierung von Rechtecken als ein Paar von Punkten (die durch eine
; Diagonale verbunden sind):

(define (konstr-rechteck p q) (cons p q))

(define (diag-punkt-1 rechteck) (car rechteck))

(define (diag-punkt-2 rechteck) (cdr rechteck))

(define (anti-diag-1 rechteck)
  (konstr-punkt (x-koord (diag-punkt-1 rechteck))
                (y-koord (diag-punkt-2 rechteck))))

(define (anti-diag-2 rechteck)
  (konstr-punkt (x-koord (diag-punkt-2 rechteck))
                (y-koord (diag-punkt-1 rechteck))))

(define (flaeche r)
  (*
    (abs (- (x-koord (diag-punkt-1 r)) (x-koord (diag-punkt-2 r))))
    (abs (- (y-koord (diag-punkt-1 r)) (y-koord (diag-punkt-2 r))))))

(define (umfang r)
  (* 2 
    (+ (abs (- (x-koord (diag-punkt-1 r)) (x-koord (diag-punkt-2 r))))
    (abs (- (y-koord (diag-punkt-1 r)) (y-koord (diag-punkt-2 r)))))))

(define (gleiche-re? re-1 re-2)
  (or
    (and (gleiche-punkte? (diag-punkt-1 re-1) (diag-punkt-1 re-2))
         (gleiche-punkte? (diag-punkt-2 re-1) (diag-punkt-2 re-2)))
    (and (gleiche-punkte? (diag-punkt-1 re-1) (diag-punkt-2 re-2))
         (gleiche-punkte? (diag-punkt-2 re-1) (diag-punkt-1 re-2)))
    (and (gleiche-punkte? (diag-punkt-1 re-1) (anti-diag-1 re-2))
         (gleiche-punkte? (diag-punkt-2 re-1) (anti-diag-2 re-2)))
    (and (gleiche-punkte? (diag-punkt-1 re-1) (anti-diag-2 re-2))
         (gleiche-punkte? (diag-punkt-2 re-1) (anti-diag-1 re-2)))))

(define (drucke-rechteck r)
  (drucke-punkt (diag-punkt-1 r))
  (display " -------- ")
  (drucke-punkt (anti-diag-2 r))
  (newline)
  (display "   |                |  ")
  (newline)
  (display "   |                |  ")
  (newline)
  (display "   |                |  ")
  (newline)
  (drucke-punkt (anti-diag-1 r))
  (display " -------- ")
  (drucke-punkt (diag-punkt-2 r))
  (newline))

; Beispiele vor Rechtecken:

(define p (konstr-punkt 0 1))

(define q (konstr-punkt 1 0))

(define r (konstr-rechteck p q))
