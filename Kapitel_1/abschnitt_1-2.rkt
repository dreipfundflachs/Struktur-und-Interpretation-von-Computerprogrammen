;;;;;;;;;;;;;;;;;;;;;;;;;;
;  SICP - Abschnitt 1.2  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fakultaet n) 
  (if (= n 1)
    1
    (* n (fakultaet (- n 1)))))

(define (fakltaet' n)
  (fak-iter 1 1 n))

(define (fak-iter produkt zaehler max-zaehler)
  (if (> zaehler max_zaehler)
  produkt
  (fak-iter (* zaehler produkt)
            (+ zaehler 1)
            max_zaehler)))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))


(define (fibo n)
    (define (fib-iter a b zaehler)
      (if (= zaehler 0)
        b
        (fib-iter (+ a b) a (- zaehler 1))))
  (fib-iter 1 0 n)
    )

(define (potenz b n)
  (if (= n 0)
    1
    (* b (potenz b (- n 1)))))

(define (potenz b n)
  (pot-iter b n 1))

(define (pot-iter b zaehler produkt)
  (if (= zaehler 0)
    produkt
    (pot-iter b (- zaehler 1) (* b produkt))))

(define (schnell-pot b n)
  (cond ((= n 0) 1)
        ((gerade? n) (quadrat (schnell-pot b (/ n 2))))
        (else (* b (schnell-pot b (- n 1))))))

(define (gerade? n) (= (remainder n 2) 0))

(define (quadrat x) (* x x))
