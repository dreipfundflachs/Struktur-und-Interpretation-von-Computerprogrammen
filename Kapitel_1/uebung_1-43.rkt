;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.43 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (identitaet x) x)

(define (komposition g f)
  (lambda (x) (g (f x))))

(define (viel-fach f n)
  (cond ((= n 0) identitaet)
        ((> n 0) (komposition f (viel-fach f (- n 1))))
        (else error "Das zweite Argument n muss >= 0 sein!")))

; Beispiel:

(define (quadrat x) (* x x))

(display ((viel-fach quadrat 2) 5))

(newline)
