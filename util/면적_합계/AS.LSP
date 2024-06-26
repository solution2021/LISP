
; acad2010에서 잘안되는 문제를 수정했습니다. 
; edited by 허은재 


(defun c:AS ()
;/ z ss sno no tno sum x entity etype num h pt ent_sum )
       (setvar "cmdecho" 1)
       (setq sno 0)
       (setq ss (ssget))
       (setq sno (sslength ss))
       (setq no 0)
       (setq sum 0.0)
       (setq tno 0)
       (command "area" "O" "")
       (setq ent_sum 0)
       (repeat sno
           (setq x (ssname ss no))
           (setq etype (cdr (assoc 0 (entget x))))
           (if (or (= etype "POLYLINE")(= etype "LWPOLYLINE"))
               (progn
                  (command "area" "e" x)
                  (setq ent_sum (getvar "area"))
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
       (princ "\nAdding ")
       (princ tno)
       (princ " \nTexts: ")
       (princ "\nThe Result is: ")
       (princ sum)
       (princ "   ")
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
               (command "text"
                        (setq z pt)
                        (setq z h)
                        (setq z 0.0)
                        (setq z sum)
               )
           )
       )
       (setvar "cmdecho" 1)
       (setvar "blipmode" 1)
       (prin1))


