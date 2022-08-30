;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 1.14 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (wechselgeld 6 5) = 5

(define (wechselgeld betrag)
  (wg betrag 5))

(define (wg betrag muenzarten)
  (cond ((= betrag 0) 1)
        ((or (< betrag 0) (= muenzarten 0)) 0)
        (else (+ (wg betrag
                     (- muenzarten 1))
                 (wg (- betrag (erster-nennwert muenzarten))
                     muenzarten)))))

(define (erster-nennwert muenzarten)
  (cond ((= muenzarten 1) 1)
        ((= muenzarten 2) 2)
        ((= muenzarten 3) 5)
        ((= muenzarten 4) 10)
        ((= muenzarten 5) 50)))

; Es seien n der Betrag und m die Münzarten. Dann ist die benötigte Anzahl von
; Schritten proportional zu n^m.
;
; Beweis: Man nehme zuerst an, dass m = 1 ist.  Dann benötigt die Berechnung
; von (wg n 1) genau 3n + 1 Schritte:
; (wg n 1) ->
; (wg (n - 1) 1) + (wg n 0) (1 + 2 = 3 Berechnungen) ->
; (wg (n - 2) 1) + (wg (n - 1) 0) (1 + 2 = 3 Berechnungen) ->
; (wg (n - 3) 1) + (wg (n - 2) 0) (1 + 2 = 3 Berechnungen) ->
; ...
; (wg 1 1) + (wg 2 0) (1 + 2 = 3 Berechnungen) ->
; (wg 0 1) + (wg 1 0) -> (1 + 2 = 3 Berechnungen) ->
; 1 = (wg 0 1)  (1 Berechnung)
;
; Man nehme jetzt an, dass die Behauptung für muenzarten = m gültig ist.
; Es sei d der Nennwert der erstein Münzart. Dann kann (wechselgeld n m) wie
; folgt berechnet werden:
;
; (wg n (m + 1)) ->
; (wg (n - d) (m + 1)) + (wg n m)
;   worin die Letztere O(n^m) Berechnungen benötigt, nach Induktion ->
; (wg (n - 2 * d) (m + 1)) + (wg (n - d) m)
;   worin die Letztere O((n - d)^m) Berechnungen benötigt ->
; (wg (n - 3 * d) (m + 1)) + (wg (n - 2 * d) m)
;   worin die Letztere O((n - 2 * d)^m) Berechnungen benötigt ->
; usw.
;
; Insgesamt brauch man also ungefähr
;   n^m + (n - d)^m + (n - 2d)^m + ... + (n % d)^m
;   = O((n / d) * n^m) = O(n^(m + 1))
; Schritte.
;
; Der Speicherbedarf ist proportional zur Tiefe des Baumes, hat also die
; Grössenordnung O(m + n). Zum Beispiel speichert der obige Rechenprozess für
; (wg 6 5) die folgende Werte:
; (wg 6 5) - (wg 6 4) - (wg 6 3) - (wg 6 2) - (wg 6 1)
;   - (wg 5 1) - (wg 4 1) - (wg 3 1) - (wg 2 1) - (wg 1 1) - (wg 0 1)
; das heißt, genau (m + n) Werte. In jedem anderem Moment der Prozess
; braucht man nur (m + n) Werte zu speichern, also hat der Speicherbedarf in
; der Tat die Grössenordnung O(m + n). 
