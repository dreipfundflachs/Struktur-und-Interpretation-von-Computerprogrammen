;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.19 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; T = T_{p, q} wird durch die folgende Matrix dargestellt:
;       [ (p + q)  q ]
;       [    q     p ]    
;
; Deshalb ist T^2 durch:
;       [ [(p + q)^2 + q^2]   (q^2 + 2pq) ]
;       [   (q^2 + 2pq)       (p^2 + q^2) ]

; dargestellt, also eine Matrix derselben Form T_{p', q'} wie T, mit:
;       p' = p^2 + q^2
;       q' = (p + q)q + p = q^2 + 2pq
;
(define (fib n)
  (if (< n 0) (error "Der Index muss positiv sein!")
  (fib-iter 1 0 0 1 n)))

(define (fib-iter a b p q zaehler)
  (cond ((= zaehler 0) b)
        ((gerade? zaehler)
         (fib-iter a
                   b
                   (+ (quadrat p) (quadrat q))
                   (+ (quadrat q) (* 2 p q))
                   (/ zaehler 2)))
         (else (fib-iter (+ (* a p) (* a q) (* b q))
                         (+ (* b p) (* a q))
                         p
                         q
                         (- zaehler 1)))))

(define (gerade? n) (= (remainder n 2) 0))

(define (quadrat x) (* x x))
