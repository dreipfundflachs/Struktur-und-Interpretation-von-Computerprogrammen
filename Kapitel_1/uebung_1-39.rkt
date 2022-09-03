;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.39 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ketten-bruch-iter n d k)
  (define (iter n d k ergebnis)
    (if (= k 0)
      ergebnis
      (iter n d (- k 1) (/ (n k) (+ (d k) ergebnis)))))
  (iter n d k 0))

(define (tan-kb x k)
  (let ((n (lambda (i)
             (if (= i 1)
               x
               (- (* x x)))))
        (d (lambda (i) (- (* 2 i) 1))))
  (ketten-bruch-iter n d k)))
