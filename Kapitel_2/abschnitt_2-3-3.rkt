;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  SICP - Abschnitt 2.3.3  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide vereinigungs-menge schnitt-menge element-der-menge? hinzufuegen-menge
         nullmenge menge-3 menge-5 menge-gerade menge-ungerade
         element-der-geordnete-menge? hinzufuegen-geordnete-mengen
         geordnete-vereinigung geordnete-schnitt-menge
         hinzufuegen-menge-als-baum element-der-menge-als-baum?)

(define (element-der-menge? x menge)
  (cond [(null? menge) false]
        [(equal? x (car menge)) true]
        [else (element-der-menge? x (cdr menge))]))

(define (hinzufuegen-menge x menge)
  (if (element-der-menge? x menge)
    menge
    (cons x menge)))

(define (schnitt-menge menge-1 menge-2)
  (cond [(or (null? menge-1) (null? menge-2))
         null]
        [(element-der-menge? (car menge-1) menge-2)
         (cons (car menge-1)
               (schnitt-menge (cdr menge-1) menge-2))]
        [else (schnitt-menge (cdr menge-1) menge-2)]))

(define (vereinigungs-menge menge-1 menge-2)
  (cond [(null? menge-1) menge-2]
        [(element-der-menge? (car menge-1) menge-2)
         (vereinigungs-menge (cdr menge-1) menge-2)]
        [else (cons (car menge-1)
                    (vereinigungs-menge (cdr menge-1) menge-2))]))

; Beispiele:

(define nullmenge null)

(define menge-3 (list 0 1 2 3))

(define menge-5 (list 0 1 2 3 4 5))

(define menge-ungerade (list 1 3 5 7 9))

(define menge-gerade (list 0 2 4 6 8 10))

; Mengen als geordnete Listen

(define (element-der-geordnete-menge? x menge)
  (cond [(null? menge) false]
        [(= x (car menge)) true]
        [(< x (car menge)) false]
        [else (element-der-geordnete-menge? x (cdr menge))]))

(define (geordnete-schnitt-menge menge-1 menge-2)
  (if (or (null? menge-1) (null? menge-2))
    null
    (let [(x1 (car menge-1))
          (x2 (car menge-2))]
      (cond [(= x1 x2) (cons x1 (geordnete-schnitt-menge (cdr menge-1)
                                                         (cdr menge-2)))]
            [(< x1 x2) (geordnete-schnitt-menge (cdr menge-1) menge-2)]
            [(> x1 x2) (geordnete-schnitt-menge menge-1 (cdr menge-2))]))))

(define (geordnete-vereinigung menge-1 menge-2)
  (cond [(null? menge-1) menge-2]
        [(null? menge-2) menge-1]
        [else (let [(x1 (car menge-1)) (x2 (car menge-2))]
                (cond [(= x1 x2) (cons x1 (geordnete-vereinigung (cdr menge-1)
                                                                 (cdr menge-2)))]
                      [(< x1 x2) (cons x1 (geordnete-vereinigung (cdr menge-1)
                                                                 menge-2))]
                      [(< x2 x1) (cons x2 (geordnete-vereinigung menge-1
                                                                 (cdr
                                                                   menge-2)))]))]))

(define (hinzufuegen-geordnete-menge x menge)
  (if (null? menge)
    (list x)
    (let [(y (car menge))]
      (cond [(= x y) menge]
            [(< x y) (cons x menge)]
            [else (cons y (hinzufuegen-geordnete-menge x (cdr menge)))]))))

; Mengen als binäre Bäume

(define (eintrag baum) (car baum))

(define (linker-ast baum) (cadr baum))

(define (rechter-ast baum) (caddr baum))

(define (konstr-baum eintrag links rechts)
  (list eintrag links rechts))

(define (element-der-menge-als-baum? x menge)
  (cond [(null? menge) false]
        [(= x (eintrag menge)) true]
        [(> x (eintrag menge))
         (element-der-menge-als-baum? x (rechter-ast menge))]
        [else (element-der-menge-als-baum? x (linker-ast menge))]))

(define (hinzufuegen-menge-als-baum x menge)
  (cond [(null? menge) (konstr-baum x null null)]
        [(= x (eintrag menge)) menge]
        [(> x (eintrag menge))
         (konstr-baum (eintrag menge)
                      (linker-ast menge)
                      (hinzufuegen-menge-als-baum x (rechter-ast menge)))]
        [else
         (konstr-baum (eintrag menge)
                      (hinzufuegen-menge-als-baum x (linker-ast menge))
                      (rechter-ast menge))]))
