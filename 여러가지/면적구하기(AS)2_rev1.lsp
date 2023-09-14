(vl-load-com)
(defun c:AS ( / sno ss no sum tno ent_sum x etype z pt h )
    (setq sno 0)
    (setq ss (ssget))
    (setq sno (sslength ss))
    (setq no 0)
    (setq sum 0.0)
    (setq tno 0)
    ;(command "area" "" "0,0" "")

    (setq ent_sum 0)

    (repeat sno
        (setq x (ssname ss no))
        (setq etype (cdr (assoc 0 (entget x))))
        (if (or (= etype "POLYLINE")(= etype "LWPOLYLINE")(= etype "CIRCLE")(= etype "SPLINE")(= etype "ELLIPSE"))
            (progn
                ;(command "area" "e" x)
                (setq ent_sum (vlax-curve-getArea  x))
                (princ ent_sum)
                ;(setq ent_sum (getvar "area"))
            )
            (progn
                (setq tno (1- tno))
                (setq ent_sum 0)
            )
        )
        (setq sum (+ sum ent_sum))
        (setq tno (1+ tno))
        (setq no (1+ no))
    )

    (setq tno (itoa tno))
    (setq sum (/ sum 1000000))
    (setq sum (rtos sum 2 2))
  (prompt (strcat "\n>> 선택 객체수 : " tno "   면적합계 : " sum))(princ)
    (princ "\nDo You Want Write it into the DWG? <y> ")
    (setq z (strcase (getstring)))
    (if (or (= z "") (= z "Y"))
        (progn
            (setq pt (getpoint "\nText Point: "))
            (setq h (getvar "textsize"))
            (prompt "\nText Height: ")
            (princ H)
            (princ "  : ")
            (setq z (getdist))
            (if z
                (setq h z)
            )
			(entmake
				(list
					(cons 0 "TEXT")
					(cons 10 pt)
					(cons 40 h)
					(cons 1 sum)
				)
			)
          	;기본 : 시작점>높이>회전>문자열
			;높이 지정 시 : 시작점>회전>문자열
          	;|
            (command "text"
                     (setq z pt)
                     (setq z h)
                     (setq z 0.0)
                     (setq z sum)
            )
            |;
        )
    )
    (prin1)
)
