;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  SICP - Abschnitt 2.2.3  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide gerade? ungerade? quadrat
         summe-ungerade-quadrate summe-ungerade-quadrate1
         fib-iter fib gerade-fibs gerade-fibs1
         filter1 abb akkumuliere
         durchzaehlen-intervall durchzaehlen-baum
         liste-quadrate-fibs produkt-der-quadrate-ungerader-elemente
         x y)

(define (gerade? n) (= 0 (modulo n 2)))

(define (ungerade? n) (not (gerade? n)))

(define (quadrat x) (* x x))

(define (summe-ungerade-quadrate1 baum)
  (cond ((null? baum) 0)
        ((not (pair? baum))
         (if (ungerade? baum) (quadrat baum) 0))
        (else (+ (summe-ungerade-quadrate1 (car baum)))
              (summe-ungerade-quadrate1 (cdr baum)))))

(define (fib-iter a b n)
  (cond ((= n 0) a)
        ((= n 1) b)
        (else (fib-iter b (+ a b) (- n 1)))))

(define (fib n)
  (fib-iter 0 1 n))

(define (gerade-fibs1 n)
  (define (naechstes k)
    (if (> k n)
      null
      (let ((f (fib k)))
        (if (gerade? f)
          (cons f (naechstes (+ k 1)))
          (naechstes (+ k 1))))))
  (naechstes 0))

(define (filter1 praedikat liste)
  (cond ((null? liste) null)
        ((praedikat (car liste)) (cons (car liste)
                                       (filter1 praedikat (cdr liste))))
        (else (filter1 praedikat (cdr liste)))))

(define (abb f liste)
  (cond ((null? liste) null)
        (else (cons (f (car liste))
                    (abb f (cdr liste))))))

(define x (list 0 1 2 3 4 5 6 7 8 9 10))

(define y (list 1 (list 2 (list 3 4)) 5))

(define (akkumuliere op anfangswert sequenz)
  (if (null? sequenz)
    anfangswert
    (op (car sequenz) (akkumuliere op anfangswert (cdr sequenz)))))

(define (durchzaehlen-intervall unten oben)
  (if (> unten oben)
    null
    (cons unten (durchzaehlen-intervall (+ unten 1) oben))))

(define (durchzaehlen-baum baum)
  (cond ((null? baum) null)
        ((not (pair? baum)) (list baum))
        (else (append (durchzaehlen-baum (car baum))
                      (durchzaehlen-baum (cdr baum))))))


(define (summe-ungerade-quadrate baum)
  (akkumuliere +
               0
               (abb quadrat 
                    (filter ungerade?
                            (durchzaehlen-baum baum)))))

(define (gerade-fibs n)
  (akkumuliere cons
               null
               (filter gerade?
                       (abb fib
                            (durchzaehlen-intervall 0 n)))))

(define (liste-quadrate-fibs n)
  (abb quadrat
       (abb fib
            (durchzaehlen-intervall 0 n))))

(define (produkt-der-quadrate-ungerader-elemente sequenz)
  (akkumuliere *
               1
               (abb quadrat
                    (filter ungerade? sequenz))))
