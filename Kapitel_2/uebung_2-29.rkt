;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.29 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide konstr-mobile linker-ast rechter-ast
         konstr-ast ast-laenge ast-struktur
         ast-gewicht gesamt-gewicht im-gleichgewicht?
         konstr-mobile-1 rechter-ast-1
         konstr-ast-1 ast-struktur-1
         mobile-1 mobile-2)

; (a)

(define (konstr-mobile links rechts) (list links rechts))

(define (konstr-ast laenge struktur) (list laenge struktur))

(define (linker-ast mobile) (car mobile))

(define (rechter-ast mobile) (cadr mobile))

(define (ast-laenge ast) (car ast))

(define (ast-struktur ast) (cadr ast))

; (b)

(define (ast-gewicht ast)
  (if (pair? (ast-struktur ast))
    (+ (ast-gewicht (linker-ast (ast-struktur ast)))
       (ast-gewicht (rechter-ast (ast-struktur ast))))
    (ast-struktur ast)))

(define (gesamt-gewicht mobile)
  (+ (ast-gewicht (linker-ast mobile))
     (ast-gewicht (rechter-ast mobile))))

; (c)

(define (im-gleichgewicht-ast? ast)
  (if (pair? (ast-struktur ast))
    (im-gleichgewicht? (ast-struktur ast))
    true))

(define (im-gleichgewicht? mobile)
  (and (= (* (ast-laenge (linker-ast mobile))
             (ast-gewicht (linker-ast mobile)))
          (* (ast-laenge (rechter-ast mobile))
             (ast-gewicht (rechter-ast mobile))))
       (im-gleichgewicht-ast? (linker-ast mobile))
       (im-gleichgewicht-ast? (rechter-ast mobile))))

; (d)
; Wir müssen die Definitionen der Selektoren 'rechter-ast' und 'ast-struktur'
; wie folgt modifizirien, aber sonst nichts anderes:

(define (konstr-mobile-1 links rechts) (cons links rechts))

(define (konstr-ast-1 laenge struktur) (cons laenge struktur))

(define (rechter-ast-1 mobile) (car mobile))

(define (ast-struktur-1 ast) (car ast))

; Beispiel eines Mobiles, das im Gleichgewicht ist:

(define mobile-1 (konstr-mobile (konstr-ast 2 3)
                                (konstr-ast 3 2))) 

; Beispiel eines Mobiles, das nicht im Gleichgewicht ist:

(define mobile-2 (konstr-mobile (konstr-ast 10 mobile-1)
                                (konstr-ast 12 5)))
