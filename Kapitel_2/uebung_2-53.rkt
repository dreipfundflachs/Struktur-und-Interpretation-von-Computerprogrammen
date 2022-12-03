;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.53 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(list 'a 'b 'c)
; ->    (a b c)

(list (list 'george))
; ->    ((george))

(cdr '((x1 x2) (y1 y2)))
; ->    ((y1 y2))

(cadr '((x1 x2) (y1 y2)))
; ->    (y1 y2)

(pair? (car '(eine kurze liste)))
; ->    #f

(memq 'rot '((rot schuhe) (blau socken)))
; ->    #f

(memq 'rot '(rot schuhe blau socken))
; ->    (rot schuhe blau socken)
