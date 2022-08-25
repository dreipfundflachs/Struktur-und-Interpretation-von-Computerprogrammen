;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.20 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ggt a b)
         (if (= 0 b)
           a
           (ggt b (remainder a b))))

; Wir werden im Folgenden die Abkürzungen: "remainder" -> "rem" und
; "Auswertung" -> "Ausw." benutzen, um die Länge der Ausdrücke zu begrenzen.
;
; (a) Normale Reihenfolge:
;
; (ggt 206 40) -> {Ausw. von ggt}
;
; (if (= 40 0) ...) -> {Ausw. von if}
;
; (ggt 40 (rem 206 40)) -> {Ausw. von ggt}
;
; (if (= 0 (rem 206 40)) ... ) -> {Ausw. von if}
;   [Eine Auswertung von remainder]
;
; (ggt (rem 206 40) (rem 40 (rem 206 40))) -> {Ausw. ggt}
;
; (if (= 0 (rem 40 (rem 206 40))) ... ) -> {Ausw. von if}
;   [Zwei Auswertungen von remainder]
;
; (ggt (rem 40 (rem 206 40)) (rem (rem 206 40) (rem 40 (rem 206 40))))
;   -> {Ausw. von ggt}
;
; (if (= 0 (rem (rem 206 40) (rem 40 (rem 206 40)))) ... ) -> {Ausw. von if}
;   [Vier Auswertungen von remainder]
;
; (ggt (rem (rem 206 40) (rem 40 (rem 206 40)))
;      (rem (rem 40 (rem 206 40)) (rem (rem 206 40) (rem 40 (rem 206 40)))))
;      -> {Ausw. von if}
;
; (if (= 0
;        (rem (rem 40 (rem 206 40)) (rem (rem 206 40) (rem 40 (rem 206 40)))))
;        ... ) -> {Ausw. von if}
;   [Sieben Auswertungen von remainder]
;
; (rem (rem 206 40) (rem 40 (rem 206 40))) -> {zwei Reduktionen}
;   [Zwei Auswertungen von remainder]
;
; (rem 6 (rem 40 6)) -> {zwei Reduktionen}
;   [Eine Auswertung von remainder]
;
; (rem 6 4) -> {Reduktion}
;   [Eine Auswertung von remainder]
;
; 2
;
; Insgesamt werden also bei Auswertung von (ggt 206 40) in der normalen
; Reihenfolge 18 = 14 + 4 remainder-Operationen ausgeführt.
;
;
; (b) Applikative Reihenfolge:
;
; (ggt 206 40) -> {Ausw. von ggt}
;
; (if (= 0 40) ... ) -> {Ausw. von if}
;
; (ggt 40 (rem 206 40)) -> {Ausw. von remainder}
;
; (ggt 40 6) -> {Ausw. von ggt}
;
; (if (= 0 6) ... ) -> {Ausw. von if}
;
; (ggt 6 (rem 40 6)) -> {Ausw. von remainder}
;
; (ggt 6 4) -> {Ausw. von ggt}
;
; (if (= 0 4) ... ) -> {Ausw. von if}
;
; (ggt 4 (rem 6 4)) -> {Ausw. von remainder}
;
; (ggt 4 2) -> {Ausw. von ggt}
;
; (if (= 0 2) ... ) -> {Ausw. von if}
;
; (ggt 2 (rem 4 2)) -> {Ausw. von remainder}
;
; (ggt 2 0) -> {Ausw. von ggt}
;
; (if (= 0 0) ... ) -> {Ausw. von if}
;
; 2
;
; Insgesamt werden also bei Auswertung von (ggt 206 40) in applikativer
; Reihenfolge nur 4 remainder-Operationen ausgeführt.
