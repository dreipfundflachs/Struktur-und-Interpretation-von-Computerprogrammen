;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.11 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Der einzige Fall, in dem man vier statt zwei Produkte berechnen muss, ist
; der Fall, worin beide Intervalle die Zahl 0 in ihren Inneren enthalten.

(define (mul-intervall I J)
  (let ((a (untere-grenze I))
        (b (obere-grenze I))
        (c (untere-grenze J))
        (d (obere-grenze J)))
    (cond ((and (>= a 0) (>= c 0))
           (konstr-intervall (* a c) (* b d)))
          ((and (>= a 0) (<= d 0))
           (konstr-intervall (* b c) (* a d)))
          ((>= a 0)
           (konstr-intervall (* b c) (* b d)))
          ((and (<= b 0) (>= c 0))
           (konstr-intervall (* a d) (* b c)))
          ((and (<= b 0) (<= d 0))
           (konstr-intervall (* b d) (* a c)))
          ((<= b 0)
           (konstr-intervall (* a d) (* a c)))
          ((>= c 0)
           (konstr-intervall (* a d) (* b d)))
          ((<= d 0)
           (konstr-intervall (* b c) (* a c)))
          (else
           (let ((p1 (* (untere-grenze I)    (untere-grenze J)))
                 (p2 (* (untere-grenze I)    (obere-grenze J)))
                 (p3 (* (obere-grenze I)     (untere-grenze J)))
                 (p4 (* (obere-grenze I)     (obere-grenze J))))
             (konstr-intervall (min p1 p2 p3 p4) (max p1 p2 p3 p4))))
          )))

(define (konstr-intervall a b)
  (cons a b))

(define (untere-grenze I) (car I))

(define (obere-grenze I) (cdr I))


(define (drucke-intervall I)
  (display "[")
  (display (untere-grenze I))
  (display " , ")
  (display (obere-grenze I))
  (display "]"))

; Beispiele von Intervallen:

; I = [1, 2]
(define I (konstr-intervall 1 2))
 
; J = [-3, -2]
(define J (konstr-intervall (- 3) (- 2)))

; K = [-4, 5]
(define K (konstr-intervall (- 4) 5))
