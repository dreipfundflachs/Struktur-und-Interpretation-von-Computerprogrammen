(define (abs x)
    (cond
      ((> x 0) x)
      ((< x 0) (- x))
      ((= x 0) 0)
      )
    )

(define (f x) 1)

(define (aabs x)
  (if (> x 0) x (- x))
  )
