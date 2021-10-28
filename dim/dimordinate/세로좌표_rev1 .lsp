;;      Made By..........   TaeEun  -_-)=b
;;
;;          Version..........   v1.0 : 120417 - Created and Designed
;;
;;          Contact Me.......   arin9916@naver.com
;;                    .......   http://cafe.naver.com/ptelisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:01
	( / doc sys OOv p1 p2 di xy oucs
	    PTE:start PTE:end PTE:memoVar PTE:CreatUcs *error*
	)

    ;; Sub Function - 01
	(defun PTE:start( lst ) 
		(vla-startundomark (PTE:end nil))
		(list lst (mapcar 'getvar lst))
	)

    ;; Sub Function - 02
	(defun PTE:end ( d / doc )
		(setq doc (vla-get-activedocument (vlax-get-acad-object)))
		(and (cadr d) (mapcar 'setvar (car d) (cadr d)))
		(and (= 8 (logand 8 (getvar 'UNDOCTL))) (vla-endundomark doc)) doc
	)

    ;; Sub Function - 03
	(defun PTE:memoVar ( va f m s / v )
		(setq v (if (eval va) (eval va) s))
		(mapcar 'princ (list "\n" m " <" v "> : "))
		(set va ( f ))
		(if (not (eval va)) (set va v)) (eval va) 
	)
	
    ;; Sub Function - 04
    (defun PTE:CreatUcs ( doc pt name f / u )
        (setq u
            (vlax-invoke (vla-get-usercoordinatesystems doc) 'add  
                pt (polar pt 0 1) (polar pt (* pi 0.5) 1) name
            )
        ) (if f (vla-put-activeucs doc u))
    )

    ;; Error Function
	(defun *error* (s) 
		(if OOv (PTE:end OOv))
		(if oucs (PTE:CreatUcs doc oucs "PTEoucs" t))
	)
	
	;==================================================================================
	;	Main function
	;==================================================================================
    (if (= 0 (getvar 'WORLDUCS))
        (progn
            (setq oucs (getvar 'UCSORG))
            (vl-cmdf "_.ucs" "_w")
        )
    )

  (setq s_td (getvar "DIMTAD"))
  (setq td (getvar "DIMTAD"))
  (setq w_count 1)
  
  (while (= w_count 1)
    (setq td (atoi (getstring (strcat "\n치수 문자 위치 [중간(0) / 위(1)] <" (itoa td) ">: "))))
    (if (or(= td 0)(= td 1))
        (progn
          (setvar "DIMTAD" td)
          (setq w_count 0)
        )
        (progn
          (prompt "0 과 1 만 선택바랍니다.\n")
        )
    )
  )
  
	(setq doc  (vla-get-activedocument (vlax-get-acad-object))
		  sys '("CLAYER" "ORTHOMODE" "DIMTAD" "DIMTVP" "CECOLOR" "ERRNO")
		  OOv  (PTE:start sys)
	)
	
	(mapcar 'setvar sys (list "0" 1 td 0 "7" 0))
	(PTE:memoVar '#dimordinateOrign getpoint "Origin Point" '(0 0 0))
	
	(command "_.ucs" "_n" #dimordinateOrign)
  
    (setq p1 (getpoint "\n▶ 방향을 지정하세요 : ")
		  p2 (getpoint p1)
		  di (mapcar '- p1 p2)
		  xy (if (< (abs(car di)) (abs(cadr di))) "_x" "_y")
	)
	
	(command "_.dim1" "_ord" "_none" p1 xy "_none" p2 "")

  (while (= nil)
      (command "dimcontinue" pause)
	)
	(if oucs (PTE:CreatUcs doc oucs "PTEoucs" t))
  (setvar "DIMTAD" s_td)
	(PTE:end OOv)(princ)
)(vl-load-com)