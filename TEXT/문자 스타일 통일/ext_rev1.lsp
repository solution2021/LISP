(defun c:ext ()
  (setvar "CMDECHO" 0)
   (setq d 0) 
   (setq e (entnext))
   (while e
       ;(print (entget e))
       (setq s  (cdr (assoc 0 (entget e))))
       (setq tt (cdr (assoc 7 (entget e))))
     
       (IF (/= NIL tt)
       	 (SETVAR "TEXTSTYLE" (cdr (assoc 7 (entget e))))
	 (SETVAR "FONTALT" "txt.shx")
	 ;	 (SETVAR "FONTALT" "whgtxt.shx")
	)
     
           
       (if (= "TEXT" s)
	        
         (command "-style" (cdr (assoc 7 (entget e))) "txt.shx , visiohg.shx" "" "" "" "N" "N" "N")
	 

	  (setq d (1+ d))
		     
     	)
	 
       ;(terpri)
       (setq e (entnext e))
     
   )
   (command "regen")
  )


;(command "-style" "HSW"   "txt.shx,whgtxt.shx" ""

		  ; "" "" ""  "n" "n" "n" "")
