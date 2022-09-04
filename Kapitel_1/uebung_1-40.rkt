;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.40 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (kubik x) (* x x x))

(define (quadrat x) (* x x))

(define (kubisch a b c)
  (lambda (x)
    (+ (kubik x)
       (* a (quadrat x))
       (* b x)
       c)))

(define dx 0.00001)

(define epsilon 0.000001)

(define (ableitung g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((ableitung g) x)))))

(define (newton g schaetzung toleranz)
  (fixpunkt (newton-transform g) schaetzung toleranz))

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
