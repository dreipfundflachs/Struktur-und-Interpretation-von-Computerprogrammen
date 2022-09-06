;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.32 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (untermengen m)
  (if (null? m)
    (list null)
    (let ((restm (untermengen (cdr m))))
      (append restm (map (lambda (teilmenge)
                           (cons (car m) teilmenge)) restm)))))

(define liste1 (list 1 2 3))
