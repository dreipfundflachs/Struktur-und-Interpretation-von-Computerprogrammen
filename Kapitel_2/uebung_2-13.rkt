;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Lösung zur Übung 2.13 - SICP  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Man betrachte zwei Intervalle:
;
;   I_1 = [m_1 - p_1 m_1 , m_1 + p_1 m_1] = [m_1 (1 - p_1) , m_1 (1 + p_1)]
;   I_2 = [m_2 - p_2 m_2 , m_2 + p_2 m_2] = [m_2 (2 - p_2) , m_2 (2 + p_2)]
;
; Hier ist m_j der Mittelpunkt von I_j und p_j sein in Prozent angegebene
; Toleranz. Wenn beide untere Grenze positiv sind, dann ist
;
;   I_1 * I_2 = [m_1 m_2 (1 - (p_1 + p_2) + p_1 p_2) ,
;                m_1 m_2 (1 + (p_1 + p_2) + p_1 p_2)].
;
; Wenn wir dazu voraussetzen, dass die Toleranzen p_j klein sind, dann ist
; p_1 p_2 vernachlässigbar. In diesem Fall sind (m_1 * m_2) eine gute Näherung
; für den Mittelpunkt von I_1 * I_2 und (p_1 + p_2) eine gute Näherung für
; seine in Prozent angegebene Toleranz.
