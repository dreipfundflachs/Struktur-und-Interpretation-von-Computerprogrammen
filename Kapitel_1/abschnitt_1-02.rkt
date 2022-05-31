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
