;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.25 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Ursprüngliche Definition von potmod:

(define (quadrat x) (* x x))

(define (gerade? n) (= 0 (remainder n 2)))

(define (potmod basis exponent m)
  (cond ((= exponent 0) 1)
        ((gerade? exponent) (remainder
                              (quadrat (potmod basis (/ exponent 2) m))
                              m))
        (else (remainder (* (potmod basis (- exponent 1) m) basis)
                         m))))

; Vorschlag von Alyssa P. Hacker:

(define (neue-potmod basis exponent m)
  (remainder (schnell-pot basis exponent) m))

(define (schnell-pot basis exponent)
  (cond ((= exponent 0) 1)
        ((gerade? exponent) (quadrat  (schnell-pot basis (/ exponent 2))))
        (else (* basis (schnell-pot basis (- exponent 1))))))

; Der Unterschied zwischen den beiden Prozeduren ist, dass die von Alyssa P.
; Hacker vorgeschlagene Version den Rest der Division von basis^exponent durch
; m nur am _Ende_ bestimmt wird. Das ist ein Nachteil wenn die Zahlen sehr groß
; im Verhältnis zu m sind, denn in der ersten Version der Prozedur arbeitet
; man nur mit Zahlen, die nicht größer als (m - 1)^2 sind. Man kann den
; Unterschied zwischen die Rechenzeit der beiden Algorithmen durch die
; Durchfürung der folgende zwei Beispielen sehen:

; (potmod 3 10000000 7)

; (neue-potmod 3 10000000 7)
