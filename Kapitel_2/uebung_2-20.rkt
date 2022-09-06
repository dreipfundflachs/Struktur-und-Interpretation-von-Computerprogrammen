;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.20 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gerade? n) (= 0 (remainder n 2)))

(define (ungerade? n) (= 0 (remainder n 2)))

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
