;***************************************************************
; M2   -> BPOLY를 이용하여 폐합된 면적을 미터제곱단위로 적어줍니다.
; PY   -> BPOLY를 이용하여 폐합된 면적을 평단위로 적어줍니다.
; PLUS -> 적어진 면적을 더해줍니다.
;         (수치를 선택할때는 w, c, f, wc, wp, filter를 이용가능)
;                        ---- 누구든 맘껏 사용하세요
;                        ---- 안윤영 1997. 9.10
;***************************************************************

(defun C:Mm (/ pt1 a1 a2 a3 h1)
;    (command "LAYER" "N" "AREA" "")
    (setq pt1 (getpoint "Pick a text location : "))
    (command "BPOLY" pt1 "")
    (setq a1 (command "AREA" "E" "l"))
    (setq a2 (getvar "area"))
    (setq a3 (rtos a2 2 2))
    (command "ERASE" "l" "")
    ;    (setq py (* 0.3025 a2))   ;단위를 평으로 할경우
;    (setq py1 (rtos py 2 4))
;    (setq pt2 (list (car pt1) (- (cadr pt1) 12)))
     (setq ds (getvar "dimscale"))
     (setq h1 (* ds 3.0)) 

    (command "TEXT" pt1 h1 "" a3)
;    (command "TEXT" pt2 h1 "" py1)
)

;====================================================
(defun C:PY (/ pt1 a1 a2 py1)
;    (command "LAYER" "N" "AREA" "")
    (setq pt1 (getpoint "Pick a text location : "))
    (command "BPOLY" pt1 "")
    (setq a1 (command "AREA" "E" "l"))
    (setq a2 (getvar "area"))
    (setq py (* 0.3025 a2))
    (setq py1 (rtos py 2 0))
    (command "ERASE" "l" "")
    (command "TEXT" pt1 "2.5" "" py1)
)

;===================================================
(defun C:PLUS (/ ent ct anx an snum slen e ee x ol nl ent2)
(command "LAYER" "N" "AREA" "")
(prompt "\nPick numbers to add :")
(setq ent (ssget))
(setq ct 0 anx 0 an 0)
(setq snum (ssname ent ct))
(setq slen (sslength ent))
(while (<= (+ 1 ct) slen)
    (setq snum (ssname ent ct))
    (setq e (entget snum))
    (setq ee (cdr (assoc 1 e)))
    (setq x (atof ee))
    (setq ol (assoc 8 e))
    (setq nl (cons 8 "ADDNUM"))
    (setq ent2 (subst nl ol e))
    (entmod ent2)
    (setq anx (+ an x))
    (setq an anx)
    (setq ct (+ 1 ct))
    )
    (setq tot (rtos anx 2 2))

    (setq pt1 (getpoint "Pick a text location : "))
    (command "TEXT" pt1 "2.5" "" tot)

;    (princ "\nTotal = ")(princ tot)(princ)
)
