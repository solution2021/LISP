(defun C:QA(/ ssent ds nent count sum scount txt ans lp nlp tpnt numtxt th nssent oper)
	(vl-load-com)
	(defun seterr (s)
		(if (/= s "함수 취소")
			(princ)
			(if (= s "quit / exit abort")
				(princ (strcat "\n에러: " s))
			)
		)
		(setq *error* oer seterr nil)
		(if (/= plist nil)
			(if (= pchk "Y") ;; 기능 취소 및 option에 poyline생성으로 설정시, 삭제되게끔 로직 작성				
				(erase)
			)
		)		
		(setvar "osmode" oms)
	)
	(setq oer *error* *error* seterr)
	(GetSetting_qa) ;; 설정값 가져오기	
	
	(setq oms (getvar "osmode"))
	
	(setvar "dimzin" 0)
	(setvar "cmdecho" 0)
	
	(setq txt 0  unctr 0 plist '() area-list '() hat_ent_lists '() cou 1 su 1 sub "add")
	
	(boun)
);defun main
	
    ;;;;;;;;;-------------------
    ;;;;;;;;; 영역 설정
    ;;;;;;;;;-------------------	

(defun boun( / ent_before)
	(setq oer *error* *error* seterr)
	(setvar "osmode" 0)
	(initget 0 "Pline Text Undo Exits Subtract Add Number Option")
	(while ((or (= p "Undo") (= p "Pline") (= p "Text")(= p "Exits")(= p "Subtract")(= p "Add")(= p "Number") (= p "Option"))
		(setq p (GETPOINT (strcat "\n영역 내를 클릭하세요. [이전(U)/폴리라인(P)/숫자기입(N)/기존문자대체(T)/옵션(O)/종료(E)](" sub "["(rtos COU 2 0)"]): " )))
		(cond
			((= p nil)
             	(redraw (car ent_list) 4)
				(text)
				(boun)
			)
			((= p "Pline")
				(Pline)
			)
			((= p "Undo")
				(undo)
				(boun)
			)
			((= p "Subtract")
				(setq su -1)
				(setq sub "Subtract")
				(boun)
			)
			((= p "Add")
				(setq su 1)
				(setq sub "Add")
				(boun)
			)
			((= p "Number")
				(setq num(getreal "\n숫자입력: "))
				(setq ans num)
				(setq area-list (append area-list (list(cons cou ans))))
				(setq plist (append plist (list(cons cou "num"))))
				(area)
				(boun)
			)
			((= p "Text")
				(text)
				(boun)
			)
			((= p "Option")
				(create_dialog_qa)				
				(dialog_setting_qa)				
				(delete_dialog_qa)
				(boun)
			)
			((= p "Exits")
				(redraw (car ent_list) 4)
				(Exits)
			)
			((= (last p) 0)
				(setq ent_before (entlast))
				(setq pll(bpoly p))
				(if (= pll nil)
					(pline)
				)
				(if (= (entget pll) nil)
					(pline)
				)
				(setq ent_list '())
				(while (setq ent_after (entnext ent_before))
					(setq ent_list (append ent_list (list ent_after)))
					(setq ent_before ent_after)
				)
				(setq ent_n (length ent_list))
				(if (>= ent_n 2)				;여집합 구간 로직
					(progn
						(setq i 0 j 1)
						(setq area_list '())
						(repeat ent_n
							(setq ent_ob (nth i ent_list))							
							(command "change" ent_ob "" "P" "C" "2" "")
							(command "area" "o" ent_ob)
							(setq ans (getvar "area"))
							(setq area_list (append area_list (list ans)))
							(setq i (+ i 1))
						)
						(vl-sort area_list '>)
						(setq sum_n (length area_list))
						(setq sum_area (nth 0 area_list))
						(repeat (- sum_n 1)
							(setq sum_area (- sum_area (nth j area_list)))
							(setq j (+ j 1))
						)
						(setq area-list (append area-list (list (cons cou sum_area))))
						(setq ans (* su sum_area))
						(setq plist (append plist (list(cons cou ent_list))))
						(repeat ent_n
							(redraw (car ent_list) 3)
							(setq ent_list (cdr ent_list))
						)
					)
					(progn
						(command "change" pll "" "P" "C" "2" "")
						(command "area" "o" pll)
						(setq ans (getvar "area"))
						(setq area-list (append area-list (list(cons cou ans))))
						(setq ans (* su ans))
						(setq plist (append plist (list(cons cou ent_list))))
						(redraw (car ent_list) 3)						
					)
				)
				(if (= Ins_hat "1")	;; 해치 생성 여부 체크
					(progn						
						(if (/= hat_patten "SOLID")
							(progn
								(command "-hatch" "P" hat_patten hat_scale "0" p "" "")
								(setq hat_ent_list (entlast))
							)
							(progn
								(command "-hatch" "P" hat_patten p "" "")
								(setq hat_ent_list (entlast))
							)
						)
						(setq hat_ent_lists (append hat_ent_lists (list (cons cou hat_ent_list))))
					)
				)				
				(area)				
				(boun)
			)
		))
	);while

);defun

;;;;;;;;;-------------------
;;;;;;;;; 객체 설정
;;;;;;;;;-------------------	

(defun pline()
	(setq oer *error* *error* seterr)
	;(setvar "osmode" 33)
	(initget 0 "Boundary Text Undo Exits Subtract Add Number Option")
	(while ((or (= p "Undo") (= p "Boundary") (= p "Text")(= p "Exits")(= p "Subtract")(= p "Add")(= p "Number") (= p "Option"))
		(setq p (entsel (strcat "\n객체를 클릭하세요. (Polyline 및 Hatch 한정) [이전(U)/영역(B)/숫자기입(N)/기존문자대체(T)/옵션(O)/종료(E)]("sub "["(rtos COU 2 0)"]): ")))
		(if (= p nil) 
			(progn 
				(text)
				(pline)
			)
		)		
		(cond
			((= p "Boundary")
				(boun)
			)

			((= p "Undo")
				(undo)
				(pline)
			)

			((= p "Text")
				(text)
				(pline)
			)
			((= p "Subtract")
				(setq su -1)
				(setq sub "Subtract")
				(pline)
			)
			((= p "Add")
				(setq su 1)
				(setq sub "Add")
				(pline)
			)
			((= p "Number")
				(setq num(getreal "\nNumber:"))
				(setq ans num)
				(setq area-list (append area-list (list(cons cou ans))))
				(setq plist (append plist (list(cons cou "num"))))
				(area)
				(pline)
			)
			((= p "Option")
				(create_dialog_qa)				
				(dialog_setting_qa)
				(delete_dialog_qa)
				(pline)
			)
			((= p "Exits")
				(Exits)
			)
			((= (cdr (assoc 0 (entget (car p)))) "LWPOLYLINE" )			
				(setq en (car p))				
				(setq plist (append plist (list(cons cou en))))				
				(command "area" "o" en)
				(setq ans (getvar "area"))
				(setq area-list (append area-list (list (cons cou ans))))
				(redraw en 3)
				(area)
				(pline)
			)
			((= (cdr (assoc 0 (entget (car p)))) "HATCH")
				(setq en1 (car p))
				(command "-hatchedit" en1 "B" "P" "N" "")
				(setq new_pl (entlast))				
				(command "area" "o" new_pl)
				(setq plist (append plist (list(cons cou (list new_pl)))))	;; 객체 삭제 리스트
				(setq ans (getvar "area"))
				(setq area-list (append area-list (list (cons cou ans))))
				(redraw new_pl 3)
				(area)
				(pline)
			)
		);cound
	)
	);while
);defun


	
;;;;;;;;;-------------------
;;;;;;;;; 객체 삭제(영역으로 설정시)
;;;;;;;;;-------------------
		
(defun erase()				;; -> 폴리선 생성에 따른 예외처리 작성해야함	
	(setq count 0)
	(setq count-1 (length plist))
	(repeat count-1
		(setq pl (nth count plist))
		(if (/= hat_ent_lists nil) (setq h1 (nth count hat_ent_lists)))
		(if (/= (cdr pl) "num")
			(progn
				(setq chk (listp (setq pl (cdr pl))))				
				(if (/= chk nil)
					(progn						
						(setq n (length pl))
						(setq h (length h1))
						(repeat n
							(if (= pchk "Y")	;; 폴리라인 객체 삭제 여부 체크
								(progn
									(entdel (car pl))
								)
								(progn			;; 폴리라인 객체가 남아있을 경우									
									(setq pent (car pl))
									(command "change" pent "" "P" "C" "bylayer" "")
									(redraw (car pl) 4)
								)
							)								
							(setq pl (cdr pl))
						)
						(if (= Del_hat "1")
							(entdel (cdr h1))
						)						
					)
					(redraw pl 4)
				)
			)
		)
		(setq count (+ count 1))
	)	
);defun

;;;;;;;;;-------------------
;;;;;;;;; 면적 합계
;;;;;;;;;-------------------	

(defun area()
	(if (= p "Undo")
		(progn
			(setq an (nth count-area area-list)
				  pl (nth count-area plist)
			)
			(if (/= (cdr pl) "num")(entdel (cadr pl)))
			(setq area-list (vl-remove an area-list)
				  plist (vl-remove pl plist)
			)			
			(setq txt (- (abs txt) (* (/ (cdr an) 1000000) sc sc)) cou (1- cou) txt_py (* txt 0.3025))
			(setq area1 (strcat "\n       제외된 선택 영역:" (rtos (* (/ (cdr an) 1000000) sc sc) 2 dpoint) "m², " (rtos (* (/ (cdr an) 1000000) sc sc 0.3025) 2 dpoint) "평"))
			(princ area1)
		);progn
		(progn
			(if (= num nil)
				(progn
					(setq area1 (strcat "\n       선택 영역:" (rtos (* (/ ans 1000000) sc sc) 2 dpoint) "m², " (rtos (* (/ ans 1000000) sc sc 0.3025) 2 dpoint) "평"))
					(setq txt (+ txt (* (/ ans 1000000) sc sc)))
					(setq txt_py (* txt 0.3025))
				)
				(progn
					(setq area1 (strcat "\n       선택 영역:" (rtos num 2 dpoint) "m², " (rtos (* num 0.3025) 2 dpoint) "평"))
					(setq txt (+ txt num))
					(setq txt_py (* txt 0.3025))
				)
			)
			(setq c (princ area1))			
			(setq unctr (1+ unctr))
			(setq cou (1+ cou))
			(setq num nil)
		)
	);if
	(setq  area2 (strcat "\n 전체 영역:" (rtos txt 2 dpoint) "m², " (rtos txt_py 2 dpoint) "평")
		   a (princ area2)
	) ; Sum Area
);defun
	
;;;;;;;;;;;--------------
;;;;;;;;;;; 뒤로 기능 
;;;;;;;;;;;--------------
  
(defun undo ()	
	(if (> unctr 0)
		(progn			
			(setq unctr (- unctr 1))
			(setq count-area(- (length area-list) 1))			
			(area)
		)
		(prompt "초기 상태입니다.")
	)
)

(defun chk-AREA()
	(if (/= ent "TEXT")
		(progn
			(prompt "\n선택한 객체가 문자가 아닙니다. ")
			(text-draw)
		)
	)
)

(defun text()
	(if (= txt 0)
		(progn
			(prompt "\n영역이 0입니다.")
			(boun)
		)
		(text-draw)
	)
)

;;;;;;;;;-------------------
;;;;;;;;; 문자 삽입 기능	
;;;;;;;;;-------------------
	
(defun text-draw()
	(setq oer *error* *error* seterr)
	(if (= cunit "N")
		(progn
			(setq txts (rtos txt 2 dpoint))
			(setq txts_py (rtos txt_py 2 dpoint))
		)
		(progn
			(setq txts (strcat (rtos txt 2 dpoint) "m²"))
			(setq txts_py (strcat (rtos txt_py 2 dpoint) "평"))
		)
	)	
	(setvar "osmode" 0)	
	(if (/= P "Text")			;;기존문자대체 기능이 아닐경우 삽입으로
		(progn
			(initget 1)
			(setq tpnt (getpoint "\n삽입할 위치를 클릭해주세요. : "))
			(setq hstyle (cdr (assoc 40 (tblsearch "style" (getvar "TEXTSTYLE")))))
			(cond
				((= kword "M")
					(if (= hstyle 0.0)
						(command "-TEXT" tpnt height "" txts "" "")
						(command "-TEXT" tpnt "" txts "" "")
					)
				)
				((= kword "P")
					(if (= hstyle 0.0)
						(command "-TEXT" tpnt height "" txts_py "" "")
						(command "-TEXT" tpnt "" txts_py "" "")
					)
				)
				((= kword "B")
					(if (= hstyle 0.0)
						(progn
							(command "-TEXT" tpnt height "" txts "" "")
							(command "-TEXT" (list (car tpnt) (- (cadr tpnt) (* height 1.2)) 0) height "" txts_py "" "")
						)
						(progn
							(command "-TEXT" tpnt "" txts "" "")
							(command "-TEXT" (list (car tpnt) (- (cadr tpnt) (* height 1.2)) 0) "" txts_py "" "")
						)
					)
				)
			)
			
			(if (= pchk "Y") (erase))
			(setq cou 1 txt 0  unctr 0 area-list '() plist '())
			(if (= p "pline")
				(pline)
				(boun)
			)
		)
		(progn
			(setq en (entsel "\n문자 선택: "))
			(if (= en nil)
				(progn
					(setq ent "LINE")
					(chk-AREA)
				)
			)
			(if (= (CDR (assoc 0 (entget (car en)))) "TEXT")
				(progn
					(cond 
						((= kword "M")
							(setq ent(cdr (assoc 0 (entget (car en)))))
							(setq new_itme (cons 1 txts))
							(setq old_itme (assoc 1 (entget (car en))))
							(setq new_ent (subst new_itme old_itme (entget (car en))))
							(entmod new_ent)
						)
						((= kword "P")
							(setq ent(cdr (assoc 0 (entget (car en)))))
							(setq new_itme (cons 1 txts_py))
							(setq old_itme (assoc 1 (entget (car en))))
							(setq new_ent (subst new_itme old_itme (entget (car en))))
							(entmod new_ent)
						)
						((= kword "B")
							(setq ent(cdr (assoc 0 (entget (car en)))))
							(setq new_itme (cons 1 txts))
							(setq old_itme (assoc 1 (entget (car en))))
							(setq new_ent (subst new_itme old_itme (entget (car en))))
							(entmod new_ent)
							
							(setq txt_xyz (cdr (assoc 10 (entget (car en)))))
							(setq height1 (cdr (assoc 40 (entget (car en)))))
							(command "-TEXT" (list (car txt_xyz) (- (cadr txt_xyz) (* height1 1.2)) 0) height1 "" txts_py "" "")
						)
					)
				)
				(chk-AREA)
			)
		)
	)
	(if (= pchk "Y") (erase))
	(setq cou 1 txt 0  unctr 0 area-list '() plist '())
)

(defun Exits()
	(setvar "osmode" oms)
	(princ "\n리습 종료")
	;(exit)
)

(defun Hatch_patten_list( / file_open line tmp lst lst_2 lst_3 lst_4)
	(vl-filename-directory (findfile "zwcad.pat"))
	(setq file_open (open (findfile "zwcad.pat") "r"))
	(while (setq line (read-line file_open))
		(setq tmp (cons line tmp))
	)
	(close file_open) 
	(setq tmp (reverse tmp))
	(setq lst (vl-remove-if-not '(lambda (string) (if (eq (substr string 1 1) "*") string)) tmp))
	(setq lst_2 (mapcar '(lambda (string) (substr string 2 (- (vl-string-search "," string) 1))) lst))
	(setq lst_3 (vl-sort lst_2 '<))
	(setq lst_4 (vl-remove "SOLID" lst_3))
	(setq lst_5 (append '("SOLID") lst_4))
)

(defun SaveSetting_qa(sc height dpoint kword pchk cunit Ins_hat Del_hat hat_patten hat_scale / file_location path lenpath op)
	(setq file_location (findfile "면적구하기 설정값.txt"))
	(if (= file_location nil)
		(progn
			(setq path (findfile "ZWCAD.CUIX"))
			(setq lenpath (strlen path))
			(setq path (substr path 1 (- lenpath 10)))
			(setq file_location (strcat path "면적구하기 설정값.txt"))
		)
	)
	(setq op (open file_location "w"))
	
	(write-line (itoa sc) op)			;; 텍스트의 첫번째 줄에 스케일 값 입력
	(write-line (rtos height 2 2) op)	;; 텍스트의 2번째 줄에 높이 값 입력
	(write-line (itoa dpoint) op)		;; 텍스트의 3번째 줄에 소수점 값 입력
	(write-line kword op)				;; 텍스트의 4번째 줄에 단위 값 입력
	(write-line pchk op)				;; 텍스트의 5번째 줄에 폴리라인 생성유무 값을 입력
	(write-line cunit op)				;; 텍스트의 6번째 줄에 단위 생성유무 값을 입력
	(write-line Ins_hat op)				;; 텍스트의 7번째 줄에 해치 삽입 여부를 설정
	(write-line Del_hat op)				;; 텍스트의 8번째 줄에 해치 삽입 후 삭제 여부 설정
	(write-line hat_patten op)			;; 텍스트의 9번째 줄에 해치 패턴 입력
	(write-line (rtos hat_scale 2 3) op);; 텍스트의 10번째 줄에 해치 패턴 스케일을 입력
	(close op)
	
	(setq sc (getvar "userr5"))
	(princ)
)

(defun GetSetting_qa( / )
	(setq fl (findfile "면적구하기 설정값.txt"))	
	(if (/= fl nil)												;; 텍스트 파일이 존재한다면.
		(progn
			(setq op (open fl "r"))
			(while (setq setting (read-line op))				;; 텍스트 값 불러오기
				(setq data (append data (list setting)))		;; 리스트형테로 data에 저장
			)
			(close op)
			(if (or (= data nil) (/= 10 (length data)))			;; 만약 텍스트에 값이 없거나 설정값이 6개가 아닌 경우	
				(progn
					(setq d_sc 1000)
					(setq ds (getvar "DIMSCALE"))
					(setq th (getvar "DIMTXT"))
					(setq d_height (* th ds))
					(setq d_dpoint 2)
					(setq d_kword "M")
					(setq d_pchk "Y")
					(setq d_cunit "N")
					(setq d_Ins_hat "0")
					(setq d_Del_hat "0")
					(setq d_hat_patten "SOLID")
					(setq d_hat_scale 1.000)
					
					(setq sc d_sc)
					(setq height d_height)
					(setq dpoint d_dpoint)
					(setq kword d_kword)
					(setq pchk d_pchk)
					(setq cunit d_cunit)
					(setq Ins_hat d_Ins_hat)
					(setq Del_hat d_Del_hat)
					(setq hat_patten d_hat_patten)
					(setq hat_scale d_hat_scale)
				)
				(progn					
					(setq sc (atoi (nth 0 data)))				;; 텍스트 1번째 줄의 값을 가져온다.
					(setq height (atof (nth 1 data)))			;; 텍스트 2번째 줄의 값을 가져온다.
					(setq dpoint (atoi (nth 2 data)))			;; 텍스트 3번째 줄의 값을 가져온다.
					(setq kword (nth 3 data))					;; 텍스트 4번째 줄의 값을 가져온다.
					(setq pchk (nth 4 data))					;; 텍스트 5번째 줄의 값을 가져온다.
					(setq cunit (nth 5 data))					;; 텍스트 6번째 줄의 값을 가져온다.
					(setq Ins_hat (nth 6 data))					;; 텍스트 7번째 줄의 값을 가져온다.
					(setq Del_hat (nth 7 data))					;; 텍스트 8번째 줄의 값을 가져온다.
					(setq hat_patten (nth 8 data))				;; 텍스트 9번째 줄의 값을 가져온다.
					(setq hat_scale (atof (nth 9 data)))		;; 텍스트 10번째 줄의 값을 가져온다.
				)
			)
		)
		(progn
			(setq d_sc 1000)									;; 스케일 1000으로
			(setq ds (getvar "DIMSCALE"))
			(setq th (getvar "DIMTXT"))
			(setq d_height (* th ds))							;; 문자높이
			(setq d_dpoint 2)									;; 소수점 자리 2자리
			(setq d_kword "M")									;; 단위는 M로 defualt
			(setq d_pchk "Y")									;; polyline 생성 기본 설정값 No
			(setq d_cunit "N")									;; 단위 삽입 설정값 No
			(setq d_Ins_hat "0")								;; 해치 삽입 기본 설정값 No
			(setq d_Del_hat "0")								;; 해치 삭제 여부 기본 설정값 No
			(setq d_hat_patten "SOLID")							;; 해치 패턴 기본값 Solid
			(setq d_hat_scale 1.000)							;; 해치 스케일 기본값 1.000
			
			(setq sc d_sc)
			(setq height d_height)
			(setq dpoint d_dpoint)
			(setq kword d_kword)
			(setq pchk d_pchk)
			(setq cunit d_cunit)
			(setq Ins_hat d_Ins_hat)
			(setq Del_hat d_Del_hat)
			(setq hat_patten d_hat_patten)
			(setq hat_scale d_hat_scale)
			
			(SaveSetting_qa sc height dpoint kword pchk cunit Ins_hat Del_hat hat_patten hat_scale)
		)
	)
	(setq data nil)
	(princ)
)

(defun dialog_setting_qa( / fname id );dcl_scale dcl_height dcl_dpoint dcl_pchk1 dcl_cunit1)
	(setq fname (findfile "면적구하기.dcl"))
	(setq id (load_dialog fname))
	(if (not (new_dialog "area_option" id)) (exit))
	
	(Hatch_patten_list) ;; 해치 패턴 리스트 작성
	(setq hlist_num (length lst_5))
	(setq sel_hlist_num (length (member hat_patten lst_5)))
	(setq hat_num (+ 1 (- hlist_num sel_hlist_num)))
	
	
	;;;;;;;;;;;;;;;; 해치 기능 추가 ;;;;;;;;;;;;;;;;
	(start_list "select_hatch")				
	(mapcar 'add_list lst_5)			
	(end_list)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;DCL에 설정값 삽입
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(set_tile "dcl_scale" (rtos sc 2 0))
	(set_tile "dcl_txt_height" (rtos height 2 2))
	(set_tile "dcl_dpoint" (itoa dpoint))
	(set_tile "Insert_hatch" Ins_hat)
	(set_tile "Delete_Hatch" Del_hat)
	(set_tile "select_hatch" (itoa hat_num))
	(set_tile "dcl_hat_scale" (rtos hat_scale 2 3))
	
	(cond 
		((= kword "M")
			(set_tile "dcl_m" "1")
			(set_tile "dcl_py" "0")
			(set_tile "dcl_both" "0")
		)
		((= kword "P")
			(set_tile "dcl_m" "0")
			(set_tile "dcl_py" "1")
			(set_tile "dcl_both" "0")
		)
		((= kword "B")
			(set_tile "dcl_m" "0")
			(set_tile "dcl_py" "0")
			(set_tile "dcl_both" "1")
		)
	)
	(if (= pchk "Y")
		(set_tile "dcl_toggle1" "1")
		(set_tile "dcl_toggle1" "0")
	)
	(if (= cunit "Y")
		(set_tile "dcl_toggle2" "1")
		(set_tile "dcl_toggle2" "0")
	)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;DCL 설정값 가져오기
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(action_tile "dcl_scale" "(setq dcl_scale $value )")
	(if (= dcl_scale nil)
		(setq dcl_scale (get_tile "dcl_scale"))
	)
	(action_tile "dcl_txt_height" "(setq dcl_height $value )")
	(if (= dcl_height nil)
		(setq dcl_height (get_tile "dcl_txt_height"))
	)
	(action_tile "dcl_dpoint" "(setq dcl_dpoint $value )")
	(if (= dcl_dpoint nil)
		(setq dcl_dpoint (get_tile "dcl_dpoint"))
	)

	(setq dcl_kword_m (get_tile "dcl_m"))
	(setq dcl_kword_py (get_tile "dcl_py"))
	(setq dcl_kword_both (get_tile "dcl_both"))
	
	(action_tile "dcl_m" "(setq dcl_kword_m $value dcl_kword_py \"0\" dcl_kword_both \"0\")")
	(action_tile "dcl_py" "(setq dcl_kword_py $value dcl_kword_m \"0\" dcl_kword_both \"0\")")
	(action_tile "dcl_both" "(setq dcl_kword_both $value dcl_kword_m \"0\" dcl_kword_py \"0\")")
	
	(setq dcl_pchk (get_tile "dcl_toggle1"))
	(setq dcl_cunit (get_tile "dcl_toggle2"))
	
	(action_tile "dcl_toggle1" "(setq dcl_pchk $value)")
	(action_tile "dcl_toggle2" "(setq dcl_cunit $value)")
	
	(if (= Ins_hat "0")
		(progn
			(mode_tile "Insert_hatch" 0)
			(mode_tile "Delete_Hatch" 1)
			(mode_tile "select_hatch" 1)
			(mode_tile "dcl_hat_scale" 1)
		)
		(progn
			(mode_tile "Insert_hatch" 0)
			(mode_tile "Delete_Hatch" 0)
			(mode_tile "select_hatch" 0)
			(mode_tile "dcl_hat_scale" 0)
		)
	)
	
	(action_tile "Insert_hatch" "(setq dcl_Ins_hat $value) (if (= dcl_Ins_hat \"1\") (progn (mode_tile \"Delete_Hatch\" 0) (mode_tile \"select_hatch\" 0) (mode_tile \"dcl_hat_scale\" 0)) (progn (mode_tile \"Delete_Hatch\" 1) (mode_tile \"select_hatch\" 1) (mode_tile \"dcl_hat_scale\" 1)))")	
	(action_tile "select_hatch" "(setq dcl_hat_patten (atoi $value))")
	(action_tile "Delete_Hatch" "(setq dcl_Del_hat $value)")
	(action_tile "dcl_hat_scale" "(setq dcl_hat_scale $value )")
	
	(if (= dcl_Ins_hat nil)
		(setq dcl_Ins_hat (get_tile "Insert_hatch"))
	)	
	(if (= dcl_hat_patten nil)
		(setq dcl_hat_patten (atoi (get_tile "select_hatch")))
	)
	
	(if (= dcl_Del_hat nil)
		(setq dcl_Del_hat (get_tile "Delete_Hatch"))
	)
	(if (= dcl_hat_scale nil)
		(setq dcl_hat_scale (get_tile "dcl_hat_scale"))
	)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	(setq ok (start_dialog))
	(if (= ok 1)
		(progn
			(cond
				((= dcl_kword_m "1")
					(setq dcl_kword "M")
				)
				((= dcl_kword_py "1")
					(setq dcl_kword "P")
				)
				((= dcl_kword_both "1")
					(setq dcl_kword "B")
				)
			)
			
			(if (= dcl_pchk "1") (setq dcl_pchk1 "Y") (setq dcl_pchk1 "N"))
			(if (= dcl_cunit "1") (setq dcl_cunit1 "Y") (setq dcl_cunit1 "N"))			
			
			(setq sc (atoi dcl_scale))
			(setq height (atof dcl_height))
			(setq dpoint (atoi dcl_dpoint))
			(setq kword dcl_kword)
			(setq pchk dcl_pchk1)
			(setq cunit dcl_cunit1)			
			(setq Ins_hat dcl_Ins_hat)			
			(setq Del_hat dcl_Del_hat)			
			(setq hat_patten (nth dcl_hat_patten lst_5))			
			(setq hat_scale (atof dcl_hat_scale))
			
			(SaveSetting_qa sc height dpoint kword pchk cunit Ins_hat Del_hat hat_patten hat_scale)
		)
	)
	(unload_dialog id)
	(princ)
)

(defun create_dialog_qa( / path lenpath file_location op)
	(setq path (findfile "ZWCAD.CUIX"))
	(setq lenpath (strlen path))
	(setq path (substr path 1 (- lenpath 10)))
	(setq file_location (strcat path "면적구하기.dcl"))
	(setq op (open file_location "w"))

	(write-line "area_option : dialog
	{ label = \"면적구하기 옵션\";
	: column
	{
		fixed_width = true;
		: boxed_column 
		{
			label = \"출력값 설정\";
			aligment = centered;
			fixed_width = true;			
			children_alignment = centered;
			: row
			{
				children_aligment = right;
				: text
				{
					label = \"축척 1: \";
					fixed_width = true;
				}
				: edit_box
				{
					key = \"dcl_scale\";
					fixed_width = true;
				}
			}
			
			: row
			{
				children_aligment = right;
				: text
				{
					label = \"문자 크기 \";
					fixed_width = true;
				}
				: edit_box
				{
					key = \"dcl_txt_height\";
					fixed_width = true;
				}
			}
			
			: row
			{
				children_aligment = right;
				: text
				{
					label = \"소수점 표기\";
					fixed_width = true;
				}
				: edit_box
				{
					key = \"dcl_dpoint\";
					fixed_width = true;
				}
			}
		}
		spacer_1;
		: boxed_radio_row
		{
			aligment = centered;			
			children_aligment = centered;
			width = 10;
			fixed_width = true;
			label = \"단위 표현방법\";			
			: radio_button
			{
				label = \"미터\";
				key = \"dcl_m\";
			}
			: radio_button
			{
				label = \"평\";
				key = \"dcl_py\";
			}
			: radio_button
			{
				label = \"둘다\";
				key = \"dcl_both\";
			}
		}
		spacer_1;
		: boxed_column
		{
			label = \"HATCH 기능\";			
			: column
			{
				fixed_width = true;
				: toggle
				{
					label = \"면적 산출 중 HATCH 기입\";
					key = \"Insert_hatch\";
					fixed_width = true;
				}
				
				: toggle
				{
					label = \"산출 후 HATCH 삭제\";
					key = \"Delete_Hatch\";
				}
			}
			
			: popup_list
			{
				label = \"패턴 : \";
				key = \"select_hatch\";					
				width = 30;
			}
			
			: row
			{
				children_aligment = left;
				: text
				{
					label = \"축척 :\";
					fixed_width = true;
				}
				: edit_box
				{
					aligment = left;
					key = \"dcl_hat_scale\";
					width = 25;
				}
			}
		}
		:toggle 
		{
			label = \"면적 산출 후 경계선(Boundary) 삭제\";
			key = \"dcl_toggle1\";			
		}
		
		:toggle 
		{
			label = \"단위 문자 삽입\";
			key = \"dcl_toggle2\";			
		}		
	}
	ok_cancel;
	}" op)
	(close op)
)
(defun delete_dialog_qa( / path)
	(vl-load-com)
	(setq path (findfile "면적구하기.dcl"))
	(vl-file-delete path)
	(princ)
)