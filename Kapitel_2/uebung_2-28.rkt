;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.28 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define x (list 0 (list 1 2) (list 3 4) 5))

(define (blaetter liste)
  (cond ((null? liste) null)
        ((not (pair? liste)) (list liste))
        (else (append (blaetter (car liste)) (blaetter (cdr liste))))))

; Beispiele:

(display (blaetter x))
(newline)

(display (blaetter (list x x)))
(newline)
