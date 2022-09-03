;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.37 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; a.

(define (ketten-bruch n d k)
  (if (= k 1)
  (/ (n 1) (d 1))
  (let ((neues-d (lambda (i)
                   (if (< i (- k 1))
                     (d i)
                     (/ (+ (* (d k) (d (- k 1))) (n k)) (d k))))))
     (ketten-bruch n neues-d (- k 1)))))

(define (berechne-phi k) (/ 1 (ketten-bruch (lambda (i) 1.0)
                                       (lambda (i) 1.0)
                                       k)))

(define eps 0.0001)

(define (nah-genug? x y) (< (abs (- x y)) eps))

(define (welches-k?)
  (define (iter k)
    (if (nah-genug? (berechne-phi k) (berechne-phi (+ k 1)))
      k
      (iter (+ k 1))))
  (iter 1))

; Antwort auf a: (welches-k) = 12


; b. 

(define (ketten-bruch-iter n d k)
  (define (iter n d k ergebnis)
    (if (= k 0)
      ergebnis
      (iter n d (- k 1) (/ (n k) (+ (d k) ergebnis)))))
  (iter n d k 0))

(define (berechne-phi-iter k)
  (/ 1
     (ketten-bruch-iter (lambda (i) 1.0)
                        (lambda (i) 1.0)
                        k)))
