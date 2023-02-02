;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.63 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide baum->liste-1 baum->liste-2
         eintrag linker-ast rechter-ast konstr-baum
         baum-1 baum-2 baum-3)

(define (baum->liste-1 baum)
  (if (null? baum)
    null
    (append (baum->liste-1 (linker-ast baum))
            (cons (eintrag baum) (baum->liste-1 (rechter-ast baum))))))

(define (baum->liste-2 baum)
  (define (kopiere-in-liste baum ergebnis-liste)
    (if (null? baum)
      ergebnis-liste
      (kopiere-in-liste (linker-ast baum)
                        (cons (eintrag baum)
                              (kopiere-in-liste (rechter-ast baum)
                                                ergebnis-liste)))))
  (kopiere-in-liste baum null))


(define (eintrag baum) (car baum))

(define (linker-ast baum) (cadr baum))

(define (rechter-ast baum) (caddr baum))

(define (konstr-baum eintrag links rechts)
  (list eintrag links rechts))

; Beispiele:

(define baum-1 (list 7 (list 3 (list 1 null null) (list 5 null null))
                     (list 9 null (list 11 null null))))

(define baum-2 (list 3 (list 1 null null)
                     (list 7 (list 5 null null)
                           (list 9 null (list 11 null null)))))

(define baum-3 (list 5 (list 3 (list 1 null null) null)
                     (list 9 (list 7 null null) (list 11 null null))))

; (a) Es seien x und y willkürliche Knoten des Baums. Dann gibt es ein
; einziger Knoten z, dessen linker Ast genau eines von x und y enthält, z. B.
; x. In dieser Situation, konventieren beide Prozeduren den Baum in die einzige
; Liste, die die Eigenschaft besitzt, dass x vor y auftaucht. Deshalb liefern
; die beide Prozeduren immer dasselbe Ergebnis.
;
; (b) Es seien S(n) (bzw. T(n)) die Anzahl von Rechenschritten zur
; Konvertierung eines ausgewogenen Baums mit n Elementen in eine Liste unter
; Verwendung der Prozedur baum->liste1 (bzw. baum->liste2). Dann gelten:
;   * S(n) = (n - 1) / 2 + S((n-1) / 2) + 1 + S((n - 1) / 2);
;   * T(n) = T((n-1) / 2) + 1 + T((n-1) / 2).
; Deshalb ist S(n) in θ(n * log n), während T(n) in θ(log n) enthalten ist.
