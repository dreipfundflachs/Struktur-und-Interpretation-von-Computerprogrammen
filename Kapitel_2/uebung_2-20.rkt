;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.20 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide gerade? ungerade?
         gerade-filtern ungerade-filtern
         paritaet xs ys)

(define (gerade? n) (= 0 (remainder n 2)))

(define (ungerade? n) (not (= 0 (remainder n 2))))

(define (gerade-filtern zahlen)
  (cond ((null? zahlen) null)
        ((gerade? (car zahlen)) (cons (car zahlen)
                                      (gerade-filtern (cdr zahlen))))
        (else (gerade-filtern (cdr zahlen)))))

(define (ungerade-filtern zahlen)
  (cond ((null? zahlen) null)
        ((ungerade? (car zahlen)) (cons (car zahlen)
                                        (ungerade-filtern (cdr zahlen))))
        (else (ungerade-filtern (cdr zahlen)))))

(define (paritaet a . zahlen)
  (if (gerade? a)
    (cons a (gerade-filtern zahlen))
    (cons a (ungerade-filtern zahlen))))

(define xs (list 0 1 2 3 4 5 6 7 8 9 10))

(define ys (list 1 2 3 4 5 6 7 8 9 10))
