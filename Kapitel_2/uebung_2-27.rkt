;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.27 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide x y z reverse-all rev-all
         reverse zaehle-blaetter baum?)

; Einige Beispiele von Listen

(define x (list 0 (list 1 2) (list 3 4) 5))

(define y (list 0 1))

(define z (list (list 1 2) 3 4))

; Definition von 'reverse-all' unter Verwendung von 'map':

(define (reverse-all liste) 
  (cond 
    ((not (pair? liste)) liste)
    (else (reverse (map reverse-all liste)))))
  
; Eine Alternative:

(define (zaehle-blaetter x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (zaehle-blaetter (car x))
                 (zaehle-blaetter (cdr x))))))

(define (baum? liste) (not (= (zaehle-blaetter liste) (length liste))))

(define (rev-all liste)
  (define (iter-rev-all liste ergebnis)
    (cond ((null? liste) ergebnis)
          ((not (list? liste)) (cons liste ergebnis))
          ((not (baum? liste)) (append (reverse liste) ergebnis))
          (else (iter-rev-all (cdr liste) (cons
                                                (rev-all (car liste))
                                                ergebnis)))))
  (iter-rev-all liste null))


(define (reverse elemente)
  (define (reverse-iter elemente ergebnis)
    (if (null? elemente)
      ergebnis
      (reverse-iter (cdr elemente) (cons (car elemente) ergebnis))))
  (reverse-iter elemente null))

