
(defun Gerr(app msg)
     (defun *error* (msg)
              (if OLDERROR (setq *error* OLDERROR))
              (if OLDCMD (setvar "cmdecho" OLDCMD))
              (princ)
     )
     (if msg
             (alert (strcat " Application error: "
                          app
                          " \n\n  "
                          msg
                          "  \n"
                       )
             )
     )
)

(defun noele(dev num / olddata newdata data data1)
            (setq data (cdr (assoc num dev)))
            (setq  olddata  (assoc num dev))
            (setq  data1 (list (car data) (cadr data) 0))
            (setq  newdata (cons num data1))   
            (setq dev (subst newdata olddata dev))            
)

(defun chtime(data / rtcode)
           (setq rtcode (strcat (rtos (/ data 3600.) 2 0) ":"))
           (setq data (- data (* (fix (/ data 3600)) 3600.)))
           (setq rtcode (strcat rtcode (rtos (/ data 60.) 2 0) ":"))
           (setq data (- data (* (fix (/ data 60)) 60.)))
           (setq rtcode (strcat rtcode (rtos data 2 0)))
)

;|*********************Dim Text Change**********************************|;
(defun 3d2d_main(/ 
                      dcl_id3 selobj selnum idnum ed obj name starttime endtime
                    )
	(setq OLDCMD (getvar "cmdecho")
		OLDERROR  *error*
		*error* Gerr
	) 
	(setvar "cmdecho" 0)      

	(princ "\n>>Select Object<<")
	(setq selobj (ssget))
	(setq selnum (sslength selobj))
	(setq starttime (getvar "DATE"))
	(setq starttime (fix (* (- starttime (fix starttime)) 100000.)))  
	(setq idnum 0.)
	(repeat selnum
		(setq ed (entget (ssname selobj idnum)))
		(setq objname (cdr (assoc 0 ed)))
		(if (= objname "INSERT")
			; objname = block
			(progn
				(setq bname (tblsearch "block" (cdr (assoc 2 ed))))
				(setq ed (entget (cdr (assoc -2 bname))))
			)
		)
		; objname /= block
		(setq o38 (assoc 38 ed))
		(setq n38 (cons (car o38) 0.0))
		(setq ed (subst n38 o38 ed))
		;(setq obj (cdr (assoc 0 ed)))
		;(setq name (cdr (assoc 1 ed)))
		(setq ed (noele ed 10))
		(setq ed (noele ed 11))
		(setq ed (noele ed 13))
		(setq ed (noele ed 14))    
		(setq ed (noele ed 21)) 
		(setq ed (noele ed 30)) 
		(setq ed (noele ed 31)) 
		(entmod ed) 
		(setq idnum (1+ idnum))
		;(princ (strcat "Converting Data Num : " (rtos idnum 2 0) "(" (rtos (* (/ idnum selnum) 100) 2 0) "%)" (chr 13)))
	)
	(setvar "ELEVATION" 0)
	(setq endtime (getvar "DATE"))
	(setq endtime (fix (* (- endtime (fix endtime)) 100000.)))  
	(princ (strcat "\nElapsed Time : " (chtime (- endtime starttime)) "\nConverting End....."))
	(setq *error* OLDERROR)
	(setvar "cmdecho" OLDCMD)
	(princ)           
)
;|**********************************************************************|;

(defun C:3d2d() (3d2d_main))
(princ "\nLoading Complete......  Programed by Shin Jong-Hwa by DooSan ENG.\nType : 3d2d")
(princ)