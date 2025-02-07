;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.35 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; x ist ein Fixpunkt der Transformation x -> 1 + 1/x genau dann, wenn 
; x^2 + x - 1 = 0. Das heißt, diese Funktion hat genau zwei Fixpunkte:
;   phi = [1 + sqrt(5)] / 2     und
;   psi = [1 - sqrt(5)] / 2.

(define eps 0.00001)

(define (nah-genug? x y eps)
  (< (abs (- x y)) eps))

(define (mittelwert a b)
  (/ (+ a b) 2))

(define (fixpunkt f schaetzwert toleranz)
  (define (versuch schaetzung)
    (let ((naechstes (mittelwert (f schaetzung) schaetzung)))
      (if (nah-genug? schaetzung naechstes toleranz)
        naechstes
        (begin
          (display naechstes)
          (newline)
          (versuch naechstes)))))
  (versuch schaetzwert))

(define phi (fixpunkt (lambda (x) (+ 1 (/ 1 x))) 1.0 eps))
