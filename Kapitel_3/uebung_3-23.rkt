;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 3.23 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

; Wir implementieren eine deque (double-ended queue) als ein Paar aus zwei
; Listen (vorne und hinten). Die vordere Liste enthält die Elemente am Anfang
; der deque, und die hintere Liste enthält die Elemente am Ende der deque
; _in umgekehrter Reihenfolge_.

(provide konstr-zs-ws
         leere-zs-ws? anfang-zs-ws ende-zs-ws
         anfang-hinzufuegen-zs-ws!  ende-hinzufuegen-zs-ws!
         anfang-entfernen-zs-ws! ende-entfernen-zs-ws!)

(define (konstr-zs-ws) (mcons '() '()))

(define (anfang-zs-ws dq) (mcar dq))

(define (ende-zs-ws dq) (mcdr dq))

(define (leere-zs-ws? dq)
  (and (null? (anfang-zs-ws dq)) (null? (ende-zs-ws dq))))

(define (anfang-hinzufuegen-zs-ws! dq element)
  (mcons element (anfang-zs-ws dq)))

(define (ende-hinzufuegen-zs-ws! dq element)
  (mcons element (ende-zs-ws dq)))

(define (anfang-entfernen-zs-ws! dq)
  (set-car! dq (mcdr anfang-zs-ws)))

(define (ende-entfernen-zs-ws! dq)
  (set-car! dq (mcdr ende-zs-ws)))
