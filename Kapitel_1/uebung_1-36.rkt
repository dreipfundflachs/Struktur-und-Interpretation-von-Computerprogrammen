;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.36 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define (fixpunkt-langsam f schaetzwert toleranz)
  (define (versuch schaetzung)
    (let ((naechstes (f schaetzung)))
      (if (nah-genug? schaetzung naechstes toleranz)
        naechstes
        (begin
         (display naechstes)
         (newline)
         (versuch naechstes)))))
  (versuch schaetzwert))

(define (g x) (/ (log 1000) (log x)))

; Beispiele:

; (fixpunkt-langsam g 2 eps)

; (fixpunkt g 2 eps)
