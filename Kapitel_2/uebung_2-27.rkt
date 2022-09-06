;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.27 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define x (list 0 (list 1 2) (list 3 4) 5))

(define y (list 0 1))

(define (zaehle-blaetter x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (zaehle-blaetter (car x))
                 (zaehle-blaetter (cdr x))))))

(define (baum? liste) (not (= (zaehle-blaetter liste) (length liste))))

(define (reverse-all liste)
  (define (iter-reverse-all liste ergebnis)
    (cond ((null? liste) ergebnis)
          ((not (list? liste)) (cons liste ergebnis))
          ((not (baum? liste)) (append (reverse liste) ergebnis))
          (else (iter-reverse-all (cdr liste) (cons
                                                (reverse-all (car liste))
                                                ergebnis)))))
  (iter-reverse-all liste null))


(define (reverse elemente)
  (define (reverse-iter elemente ergebnis)
    (if (null? elemente)
      ergebnis
      (reverse-iter (cdr elemente) (cons (car elemente) ergebnis))))
  (reverse-iter elemente null))

; Eine andere Alternative, unter Verwendung von 'map':

(define (rev-all liste) 
  (if (not (pair? liste))
    liste
    (reverse (map rev-all liste))))
  

