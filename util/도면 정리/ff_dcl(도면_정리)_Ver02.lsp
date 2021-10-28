;;=====================================================================
;  도면정리하기(2007.8.26 주말농부)
;  ->Audit, Draworder, Purge,  Point<node> Delete,
;    Layer Filters Delete. Ghost delete.
;  ->ghost 수정보완/공백열만 있는건 모두삭제(2007.12.12)
;
;
; 3.16님 최초 작성(2007.12)
;
;
;  acad 2011 사용가능
;  추가수정(2011.09.25 일렉) 
;  주석삭제하기(추가)
;  솔리드뒤로보내기-HATCHTOBACK 명령으로 변경
;  dcl lisp 내 포함
;
;
;  추가수정(2014.10.20 일렉) 
;  DGN 파일 삭제
;
;  공시님 2013.01.22    
;  중복된 PLine, Vertex Line, Block, Text, Text 뒷부분 공란 삭제 추가
;
;
;  초록빛님 2014.06.30
;  DNG파일 Audit & purge 추가
; 
;
;  정길복 - 중복 text 제거하기
;
;------- file filtering -----------------------------------------------
(defun c:ff(/ tg1 tg2 tg3 tg4 tg5 tg6 tg7 tg8 tg9 tg10 tg11 tg12 tg13 tg14 ky fname) ; 임시파일 fname 만들기


	;-------------------------------------------------------------------------
	; DCL화일을 임시로 만들기
	;
	;
	; DCL 파일 & Lisp & Sld
	; 행복한하루님 참조
	; (setq fname (vl-filename-mktemp "dcl.dcl")) 
	; (setq fn (open fname "w")) 
	; (write-line "dcl내용" fn)
	; (close fn)
	;-------------------------------------------------------------------------

	(defun subDclFile (/ fname fn)
	(setq fname (vl-filename-mktemp "dcl.dcl")) 
	 (setq fn (open fname "w")) 
	 (write-line 
	   "file_filter : dialog { label=\"도면 정리 하기(Cad Files Diet)\";
	   : boxed_column {label = \"실행할 명령 선택\";
	        : column {
	            : toggle {label=\"  1. 도면감사<Audit> 하기\"; key=\"tog1\";}
 	            : toggle {label=\"  2. 솔리드 뒤로<Hatchtoback> 보내기\"; key=\"tog2\";}
	            : toggle {label=\"  3. 퍼지<Purge> 하기\"; key=\"tog3\";}
	            : toggle {label=\"  4. 주석<Scale List> 삭제 하기\"; key=\"tog4\";}
	            : toggle {label=\"  5. 포인트<Node> 삭제\"; key=\"tog5\";}
	            : toggle {label=\"  6. 레이어 필터<Filter> 삭제\"; key=\"tog6\";}
	            : toggle {label=\"  7. DGN<Audit & purge>\"; key=\"tog7\";}
	            : toggle {label=\"  8. 중복된 PLine 삭제\"; key=\"tog8\";}
	            : toggle {label=\"  9. 중복된 Vertex Line 삭제\"; key=\"tog9\";}
	            : toggle {label=\" 10. 중복된 Hetch 삭제\"; key=\"tog10\";}
	            : toggle {label=\" 11. 중복된 Text 삭제\"; key=\"tog11\";}
	            : toggle {label=\" 12. Text 뒷부분 Space 삭제\"; key=\"tog12\";}
	            : toggle {label=\" 13. 내용이 없는<Text>, 길이가 0인<Line> 유령객체 삭제\"; key=\"tog13\";}
	            : toggle {label=\" 모두 선택하기 / 모두 취소하기\"; key=\"tog14\";}
	      }
	   }
	   ok_cancel;
	}
	" fn)
	 (close fn)
         fname
	)

	;-------------------------------------------------------------------------
	; DCL 서브루틴
	;-------------------------------------------------------------------------
	(defun @ff_select(tg14)
	   (cond
	      ((= tg14 "0")(setq tg1 "0" tg2 "0" tg3 "0" tg4 "0" tg5 "0" tg6 "0" tg7 "0" tg8 "0" tg9 "0" tg10 "0" tg11 "0" tg12 "0" tg13 "0" tg14 "0"))
	      ((= tg14 "1")(setq tg1 "1" tg2 "1" tg3 "1" tg4 "1" tg5 "1" tg6 "1" tg7 "1" tg8 "1" tg9 "1" tg10 "1" tg11 "1" tg12 "1" tg13 "1" tg14 "1"))
	   )
	   (set_tile "tog1" tg1)(set_tile "tog2" tg2)
	   (set_tile "tog3" tg3)(set_tile "tog4" tg4)
	   (set_tile "tog5" tg5)(set_tile "tog6" tg6)
	   (set_tile "tog7" tg7)(set_tile "tog8" tg8)
	   (set_tile "tog9" tg9)(set_tile "tog10" tg10)
	   (set_tile "tog11" tg11)(set_tile "tog12" tg12)
	   (set_tile "tog13" tg13)(set_tile "tog14" tg14)
	)




	;-------------------------------------------------------------------------
	; 폴리라인 정점 리스트
	;-------------------------------------------------------------------------
	(defun GetPolyVtx(EntList)
	   (setq VtxList '())
	   (foreach x EntList
	      (if (= (car x) 10) (setq VtxList (append VtxList (list (cdr x)))) )
	   )
	VtxList)


	;-------------------------------------------------------------------------
	; 5. 포인트 삭제
	;-------------------------------------------------------------------------
	(defun @pointdelete(/ ss)
	   (setq ss (ssget "x" (list (cons 0 "point"))))
	   (if ss (progn (command "erase" ss "") (princ "\n->Point<node> ")
	         (princ (sslength ss)) (princ "개") (princ " 삭제하였습니다.") ))
	(prin1))



	;-------------------------------------------------------------------------
	; 6. 레이어 필터 삭제하기
	;-------------------------------------------------------------------------
	(defun @layerfilterdelete(/ objXDict)
	   (setq strKeepWC "")
	   (vl-load-com)
	   (vl-catch-all-apply
	      (function
	         (lambda ()
	            (setq objXDict (vla-GetExtensionDictionary
	                  (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object)))))))
	   )
	   (cond (objXDict
	        (or
	         (rrbI:DeleteAllXRecs objXDict "ACAD_LAYERFILTERS" strKeepWC)
	         (rrbI:DeleteAllXRecs objXDict "AcLyDictionary" strKeepWC))))
	(princ))
	(defun rrbI:DeleteAllXRecs  (objXDict dictName strKeepWC / objDict i)
	   (vl-catch-all-apply
		   (function
	      (lambda ()
	         (setq objDict (vla-Item objXDict dictName))
	         (vlax-for objXRec  objDict
	            (cond ((not (and strKeepWC (wcmatch (vla-Get-Name objXRec) strKeepWC)))
	               (setq i (1+ (cond (i)
	                              (0))))
	               (vla-Delete objXRec)))))))
		   (cond (i (princ (strcat "\n" (itoa i) " filters deleted."))))
	)
	
		

	;-------------------------------------------------------------------------
	; 7. DGN Audit & purge
	;-------------------------------------------------------------------------


	(defun DGN (/)
	  (if (dictremove (namedobjdict) "acad_dgnlinestylecomp")
	    (progn
      (princ
	 "\nClean DGN-rubbish complete.  ASAP, Audit & purge is recommended !!! ."
	      )
	    )
	    (princ
	      "\nacad DGNlinestylecomp not found."
	    )
	  )
	  (princ)
	)



	;-------------------------------------------------------------------------
	; 8. 중복된 PLine 삭제
	;-------------------------------------------------------------------------

	(defun pdd (/ a i al tt ke xy xy_list w_list k j)
	(command "cmdecho" 0)
	(command "undo" "g")
	(setq a (ssget "all" '((0 . "LWPOLYLINE"))))
	(if (null a)
	(setq a (ssget "x" '((0 . "LWPOLYLINE"))))
	)
	(if (null a) (exit))
	(setq al (sslength a)
		i 0
		j 0
		tt 0
		)

	(while (> al i)
		(setq en (ssname a i)
			ent (entget en)
			la (cdr (assoc 8 ent))
			i (1+ i)
			j (1+ j)
			w_list nil
		)

		(foreach k ent  
			(if (= 10 (car k)) (setq w_list (append w_list (list (cdr k)))))
		)
		(setq w_list (append w_list (list la)))
		(if (member w_list xy_list)
			(setq tt (1+ tt)
				a (ssdel en a)
				al (1- al)
				en (entdel en)
				i (1- i)
			)
			(setq xy_list (append xy_list (list w_list)))
		)
	)
	(princ "\n")
	(princ "\r [중복된 PLine 삭제] 전체 PLine (")(princ i) (princ ")중 - (")(princ tt)(princ ")개 삭제하였습니다. ")
	(command "undo" "e")
	(princ)
	)


	;-------------------------------------------------------------------------
	; 9. 중복된 Vertex Line 삭제
	;-------------------------------------------------------------------------

	(defun pxx ( / a i al tt ke j k k1 en ent xy_end xy)
	(command "undo" "g")
	(setvar "cmdecho" 0)
	(setvar "osmode" 0)
	(setq a (ssget "x" '((0 . "lwpolyline"))))
	(if (null a) (exit))
	(setq al (sslength a)
		i 0
		j 0
		k1 0
		tt 0
		)

	(while (> al i)
		(setq en (ssname a i)
			ent (entget en)
			j (1+ j)
			xy (cdr (assoc 10 ent))
			k1 0
		)

		(foreach k ent  
			(if (= 10 (car k)) (setq xy_end (cdr k) k1 (1+ k1)))
		)

		(if (and (<= k1 2) (equal xy xy_end))
			(setq tt (1+ tt)
				a (ssdel en a)
				al (1- al)
				en (entdel en)
			)
			(setq i (1+ i))
		)

	)
	(princ "\n")
	(princ "\r [중복된 Vertex Line 삭제] 전체 Line (")(princ i) (princ ")중 - (")(princ tt)(princ ")개 삭제하였습니다. ")
	(command "undo" "e")
	(princ)
	)


	;--------------------------------------------------------------------------------------------------
	; 10. 중복된 Hetch 삭제
	;--------------------------------------------------------------------------------------------------

	(defun get_hetch_pts(e / lst)
	 (foreach x e 
	  (if (= (car x) 10)
	   (setq lst (cons (cdr x) lst))
	  )
	 )
	 (mapcar '(lambda (x) (mapcar 'fix x)) (cdr lst))
	 )
	 
	 (defun hdd( / ss n en pts ptlst)
	 (setvar "cmdecho" 0)
	 (if (setq ss (ssget "x" '((0 . "HATCH"))))
	  (progn
	   (setq n 0)
	   (repeat (sslength ss)
		(setq     
		 en (ssname ss n)
		 pts (get_hetch_pts (entget en))
		 pts (vl-sort pts '(lambda (a b) (> (cadr a) (cadr b))))
		 pts (vl-sort pts '(lambda (a b) (< (car a) (car b))))
		)
		(if (member pts ptlst)
		 (command "erase" en "")
		 (setq ptlst (cons pts ptlst))
		)
		(setq n (1+ n))
	   )
	  )
	 )
	 (princ)
	 )


	;-------------------------------------------------------------------------
	; 11. 중복된 Text 삭제
	;    
	; 정길복 - 중복 text 제거하기
	;-------------------------------------------------------------------------

	(defun tdd ( / ss index entname_list ssn ent nn text_list ssn entt text_app text_list result leng n listobject info lengnum)
	 (setvar "cmdecho" 0)
	 (setq ss (ssget "x" '((0 . "text"))))
	 (if (/= ss nil)
	  (progn
	   (setq index 0 entname_list '() lengnum 0)
	   (setq ssn (sslength ss))   
	   (repeat ssn 
		(setq ent (entget (ssname ss index)))	
		(setq nn 0 text_list '())
		 (repeat ssn
		  (setq entt (entget (ssname ss nn)))
		  (if (and (/= index nn) (= (cdr (assoc 1 ent)) (cdr (assoc 1 entt))))
		   (progn	    
			(setq text_app (list (vl-member-if '(lambda (x) (= (cdr x) "AcDbText")) entt)))
			(setq text_list (append text_list text_app))))
			(setq nn (1+ nn)))
		(setq result (vl-position (vl-member-if '(lambda (x) (= (cdr x) "AcDbText")) ent) text_list))
		(if (/= result nil) (setq entname_list (append entname_list (list (list (cdr (assoc -1 ent)))))))
		(setq leng (length entname_list))
		(setq lengnum (/ (+ lengnum leng) 2))	
		(if (/= leng 0)
		 (progn
		  (setq n 0) 
		   (repeat leng
			(setq listobject (car (nth n entname_list)))
			(command "erase" listobject "")		
			(setq n (1+ n)))))	
		  (setq index (1+ index)))))
	 (if (/= leng 0)
	  (progn 
	   (setq info (strcat ">> 중복 문자 " (rtos (+ lengnum 1) 2 0) "개를 삭제하였습니다."))
	   (terpri) 
	   (prompt info))) 
	 (princ)
	)  
  

	;-------------------------------------------------------------------------
	; 12. Text 뒷부분 Space 삭제
	;-------------------------------------------------------------------------

	(defun kDD()
	(setvar "cmdecho" 0)
	(command "undo" "g")
	(setq ss (ssget "x" '((0 . "TEXT"))))
	(if (null ss) (exit))
	(setq ssl (sslength ss)
		i 0
		k 0)
	(while (> ssl i)
		(setq en (ssname ss i)
			ent (entget en)
			tv1 (cdr (assoc 1 ent))
			tv tv1
			i (1+ i)
		)
	(if (/= tv1 "")(progn
		(setq tl 1)
		(while (= (substr tv tl 1) " ")
				(setq tv (substr tv 2))
		)
		(if (= tv "") (entdel en))
		(if (/= tv tv1)
			(setq ent (subst (cons 1 tv) (assoc 1 ent) ent)
				ent (entmod ent)
				k (1+ k))
		)
		(setq tv tv1)
		(setq tl (strlen tv))
		(while (= (substr tv tl) " ")
				(setq tv (substr tv 1 (1- tl)))
				(setq tl (1- tl))
		)	
		(if (= tv "") (entdel en))
		(if (/= tv tv1)
			(setq ent (subst (cons 1 tv) (assoc 1 ent) ent)
				ent (entmod ent)
				k (1+ k)
			)
		)
	)
	(progn
		(setq k (1+ k))
		(entdel en)
	))
	(princ "\r [Text 뒷부분 Space 삭제] 전체 Text (")(princ i) (princ ")중 - (")(princ k)(princ ")개 수정하였습니다. ")
	)

	(command "undo" "e")
	(princ)
	)


	;-------------------------------------------------------------------------
	; 13. 유령객체 삭제
	;-------------------------------------------------------------------------
	(defun @ghost(/ k j ss ss1 en ed etn x10 x11 lis n dissum dis
	                tk tnum tss ten ted ttxt tvar @1 ttk)
	   (setvar "cmdecho" 0)
	   (prompt "\n>>내용이 없는 유형객체 삭제하기<text/mtext/line/pline/>..")
	   (setq k 0 j 0 tk 0 tnum 0)
	
	 ;내용이 없는 text -> erase
	   (setq tss (ssadd))
	   (setq ss (ssget "x" (list (cons 0 "text,mtext"))))
	   (if ss
	      (repeat (sslength ss)
	         (setq ten (ssname ss tk))
	         (setq ted (entget ten))
	         (setq ttxt (cdr (assoc 1 ted)))
	         (setq ascii_list (vl-string->list ttxt))
		         (setq ttk 0 tvar 0)
	         (repeat (length ascii_list)
	            (setq @1 (nth ttk ascii_list))
	            (if (/= @1 32) (setq tvar 1))
	            (setq ttk (1+ ttk))
	         )
	         (if (= tvar 0) (progn (ssadd ten tss) (setq tnum (1+ tnum))))
	         (setq tk (1+ tk))
	      )
	   )
	   (if (> tnum 0) (progn (command "erase" tss "") (princ "\n->내용이 없는 text ") 
	         (princ tnum) (princ "개") (princ " 삭제하였습니다.")))
	;
	   (setq ss1 (ssget "x" (list (cons 0 "line,lwpolyline"))))
	   (if ss1 (progn
	      (repeat (sslength ss1)
	         (setq en (ssname ss1 k)
	                  ed (entget en)
	                  etn (cdr (assoc 0 ed)))
	         (cond ((= etn "LINE")
	            (setq x10 (cdr (assoc 10 ed))
	                     x11 (cdr (assoc 11 ed)))
	            (if (= (distance x10 x11) 0) (progn (command "erase" en "") (setq j (1+ j)))))
	            ((= etn "LWPOLYLINE")
	               (setq lis (GetPolyVtx ed))
	               (setq n 0 dissum 0)
	               (repeat (1- (length lis))
	                  (setq dis (distance (nth n lis) (nth (1+ n) lis)))
	                  (setq dissum (+ dissum dis))
	                  (setq n (1+ n))
	               )
	               (if (= dissum 0) (progn (command "erase" en "") (setq j (1+ j))))
	            )
	         );cond
	      (setq k (+ k 1))
	      );repeat
	   ));if progn
	   (if (> j 0) (progn (princ "\n->선길이 0 인 객체 ") (princ j) (princ "개를") (princ " 삭제하였습니다.")))
	(princ))



	; 아래부터는 메인함수에서 작동되는 구문들입니다.
	;-------------------------------------------------------------------------
	;<- 메인시작
	   (prompt " 도면 정리 하기...")
       (setq fname (subDclFile)) ; 서브함수를 호출하여 dcl화일을 만든후에 fname변수에 저장
	   (setq dcl_id (load_dialog fname))
	   (setq ky 15 tg1 "1" tg2 "1" tg3 "1" tg4 "1" tg5 "1" tg6 "1" tg7 "1" tg8 "1" tg9 "1" tg10 "1" tg11 "1" tg12 "1" tg13 "1" tg14 "1")
	   (if (not (new_dialog "file_filter" dcl_id)) (exit))
	   (set_tile "tog1" tg1)(set_tile "tog2" tg2)
	   (set_tile "tog3" tg3)(set_tile "tog4" tg4)
	   (set_tile "tog5" tg5)(set_tile "tog6" tg6)
	   (set_tile "tog7" tg7)(set_tile "tog8" tg8)
	   (set_tile "tog9" tg9)(set_tile "tog10" tg10)
	   (set_tile "tog11" tg11)(set_tile "tog12" tg12)
	   (set_tile "tog13" tg13)(set_tile "tog14" tg14)
	   (action_tile "tog1" "(setq tg1 $value)")
	   (action_tile "tog2" "(setq tg2 $value)")
	   (action_tile "tog3" "(setq tg3 $value)")
	   (action_tile "tog4" "(setq tg4 $value)")
	   (action_tile "tog5" "(setq tg5 $value)")
	   (action_tile "tog6" "(setq tg6 $value)")
	   (action_tile "tog7" "(setq tg7 $value)")
	   (action_tile "tog8" "(setq tg8 $value)")
	   (action_tile "tog9" "(setq tg9 $value)")
	   (action_tile "tog10" "(setq tg10 $value)")
	   (action_tile "tog11" "(setq tg11 $value)")
	   (action_tile "tog12" "(setq tg12 $value)")
	   (action_tile "tog13" "(setq tg13 $value)")
	   (action_tile "tog14" "(setq tg14 $value)(@ff_select tg14)")
	   (action_tile "accept" "(setq ky 16)(done_dialog)")
	   (action_tile "cancel" "(done_dialog)")
	   (start_dialog)
	(if (= ky 16)(progn
	   (command "undo" "be")
	   (if (= tg1 "1") (command "audit" "y"))
	   (if (= tg2 "1") (command "hatchtoback"))	
	   (if (= tg3 "1") (command "-purge" "a" "*" "n"))
	   (if (= tg4 "1") (command "-scalelistedit" "r" "y" "e"))
	   (if (= tg5 "1") (@pointdelete))
	   (if (= tg6 "1") (@layerfilterdelete))
	   (if (= tg7 "1") (dgn))
	   (if (= tg8 "1") (pdd))
	   (if (= tg9 "1") (pxx))
	   (if (= tg10 "1") (hdd))
	   (if (= tg11 "1") (tdd))
	   (if (= tg12 "1") (kdd))
	   (if (= tg13 "1") (@ghost))
	   (command "undo" "e")
	   (command "qsave")
	   (prompt "\n>>도면정리 및 저장 완료"))
	   (prompt "\n>>도면정리취소")
	)
        (vl-file-delete fname) ;<- 임시로 만들었던 fname화일을 삭제시킵니다.
	(prin1)
	;<- 메인종료
	;-------------------------------------------------------------------------
	)(princ)

