(defun c:LLS( / p1 p2 p3 objs objs2 n obj dis tot_dis pl sc text_h num_size h w pp1 pp2 pp3 pp4 lp1 lp2 tp1 tp2 l_ent1 l_ent2 cen1 cen2 corners boxsize bw lay max_lay_len max_lay_name laylist dtr text_sort SaveSetting_clt GetSetting_clt create_dialog_clt delete_dialog_clt entlist set_entlist)

	;;;;;;;;;;;;;;;;;; drgree 를 라디안으로 ;;;;;;;;;;;;;
	(defun dtr(a)
		(* (/ a 180.0) pi)
	)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	;;;;;;;;;;;;;;;;;; 문자 정렬 함수 ;;;;;;;;;;;;;;;;;;
	(defun text_sort(obj cen / ent text_p tb ll ur mid)
		(setq ent (entget obj))
		(setq text_p (cdr (assoc 10 ent)))
		(setq tb (textbox ent))
		(setq ll (mapcar '+ (car tb) text_p))
		(setq ur (mapcar '+ ll (mapcar '- (cadr tb) (car tb))))
		(setq mid (polar ll (angle ll ur) (/ (distance ll ur) 2.0)))
		(setq dis (mapcar '- cen mid))
		(command "_move" obj "" dis "")
	)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	;;;;;;;;;;;;;;;;;;;; 설정값 저장 ;;;;;;;;;;;;;;;;;;;;;
	(defun SaveSetting_clt(sc height dpoint / file_location path lenpath op)
		(setq file_location (findfile "zwlisp_옵션.txt"))
		(if (= file_location nil)
			(progn
				(setq path (findfile "ZWCAD.CUIX"))
				(setq lenpath (strlen path))
				(setq path (substr path 1 (- lenpath 10)))
				(setq file_location (strcat path "zwlisp_옵션.txt"))
			)
		)
		(setq op (open file_location "w"))
		
		(write-line (itoa sc) op)			;; 텍스트의 첫번째 줄에 스케일 값 입력
		(write-line (rtos height 2 2) op)	;; 텍스트의 2번째 줄에 높이 값 입력
		(write-line (itoa dpoint) op)		;; 텍스트의 3번째 줄에 소수점 값 입력
		(close op)
		
		(setq sc (getvar "userr5"))
		(princ)
	)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 설정값 불러오기 ;;;;;;;;;;;;;;;;;;;;
	(defun GetSetting_clt( / )
		(setq fl (findfile "zwlisp_옵션.txt"))	
		(if (/= fl nil)												;; 텍스트 파일이 존재한다면.
			(progn
				(setq op (open fl "r"))
				(while (setq setting (read-line op))				;; 텍스트 값 불러오기
					(setq data (append data (list setting)))		;; 리스트형테로 data에 저장
				)
				(close op)
				(if (or (= data nil) (/= 3 (length data)))			;; 만약 텍스트에 값이 없거나 설정값이 6개가 아닌 경우	
					(progn
						(setq d_sc 1000)
						(setq ds (getvar "DIMSCALE"))
						(setq th (getvar "DIMTXT"))
						(setq d_height (* th ds))
						(setq d_dpoint 2)
						
						(setq sc d_sc)
						(setq height d_height)
						(setq dpoint d_dpoint)					
					)
					(progn					
						(setq sc (atoi (nth 0 data)))				;; 텍스트 1번째 줄의 값을 가져온다.
						(setq height (atof (nth 1 data)))			;; 텍스트 2번째 줄의 값을 가져온다.
						(setq dpoint (atoi (nth 2 data)))			;; 텍스트 3번째 줄의 값을 가져온다.					
					)
				)
			)
			(progn
				(setq d_sc 1000)									;; 스케일 1000으로
				(setq ds (getvar "DIMSCALE"))
				(setq th (getvar "DIMTXT"))
				(setq d_height (* th ds))							;; 문자높이
				(setq d_dpoint 2)									;; 소수점 표기
				
				(setq sc d_sc)
				(setq height d_height)
				(setq dpoint d_dpoint)			
				
				(SaveSetting_clt sc height dpoint)
			)
		)
		(setq data nil)
		(princ)
	)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	;;;;;;;;;;;;;;;;;;;;;;;;; dcl 파일 ;;;;;;;;;;;;;;;;;;;;;;;
	(defun create_dialog_clt( / path lenpath file_location op)
		(setq path (findfile "ZWCAD.CUIX"))
		(setq lenpath (strlen path))
		(setq path (substr path 1 (- lenpath 10)))
		(setq file_location (strcat path "zw_opton.dcl"))
		(setq op (open file_location "w"))

		(write-line "line_sum_option : dialog
		{ label = \"선길이 합 옵션\";
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
			}
			ok_cancel;
		}" op)
		(close op)
	)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	;;;;;;;;;;;;;;;;;;;;;;;; dcl 파일 삭제 ;;;;;;;;;;;;;;;;;;;;;;;;
	(defun delete_dialog_clt( / path)
		(vl-load-com)
		(setq path (findfile "zw_opton.dcl"))
		(vl-file-delete path)
		(princ)
	)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	;;;;;;;;;;;;;;;;;;;;;;; dialog 셋팅 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(defun dialog_setting_clt( / fname id );dcl_scale dcl_height dcl_dpoint dcl_pchk1 dcl_cunit1)
		(setq fname (findfile "zw_opton.dcl"))
		(setq id (load_dialog fname))
		(if (not (new_dialog "line_sum_option" id)) (exit))
		
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;DCL에 설정값 저장
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		(set_tile "dcl_scale" (rtos sc 2 0))
		(set_tile "dcl_txt_height" (rtos height 2 2))
		(set_tile "dcl_dpoint" (itoa dpoint))

		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;DCL 설정값 불러오기
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
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
		(setq ok (start_dialog))
		(if (= ok 1)
			(progn
				(setq sc (atoi dcl_scale))
				(setq height (atof dcl_height))
				(setq dpoint (atoi dcl_dpoint))
				
				(SaveSetting_clt sc height dpoint)
			)
		)
		(unload_dialog id)
		(princ)
	)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	(setvar "cmdecho" 0)
	(princ "\n길이를 산출할 객체를 선택하세요.")
	(setq objs (ssget '((0 . "LINE,CIRCLE,ARC,POLYLINE,LWPOLYLINE,ELLIPSE,SPLINE"))))
	(command "undo" "be")
	(GetSetting_clt)	
	(setq tot_dis 0 laylist nil set_entlist '())
	(setq max_lay_name "레이어명" max_lay_len 4)
	
	(initget 0 "Option")
	(while (= "Option" (setq p1 (getpoint (strcat "\n표 삽입 위치 : < 현재설정 : 축척=" (itoa sc) ", 문자높이=" (rtos height 2 2) ", 소수점=" (itoa dpoint) ", 옵션설정(O)>" ))))
		(create_dialog_clt)
		(dialog_setting_clt)
		(delete_dialog_clt)		
	)	
	
	(if objs
		(repeat (setq n (sslength objs))
			(setq lay (cdr (assoc 8 (entget (ssname objs (setq n (1- n)))))))
			(if (> (strlen lay) max_lay_len)
				(progn
					(setq max_lay_name lay)
					(setq max_lay_len (strlen lay))
				)
			)
			(if (not (member lay laylist)) (setq laylist (cons lay laylist)))
		)
	)
  
  	(setq laylist (vl-sort laylist '<))
  
	(foreach lay laylist
		(setq entlist '())
		(repeat (setq n (sslength objs))
			(setq ent (ssname objs (setq n (1- n))))
			(setq ent_lay (cdr (assoc 8 (entget ent))))
			(if (= ent_lay lay)				
				(setq entlist (append (list ent) entlist))
			)
		)
		(setq set_entlist (append set_entlist (list entlist)))
	)
	;(setq lay (list "LINE" "CIRCLE" "ARC" "POLYLINE" "LWPOLYLINE" "ELLISPE" "SPLINE"))
	
	(setq corners (textbox (list (cons 1 max_lay_name) (cons 40 height))))
    (setq boxsize (mapcar '- (cadr corners) (car corners)))
    (setq bw (fix (car boxsize)))
	
	(setq hstyle (cdr (assoc 40 (tblsearch "style" (getvar "TEXTSTYLE"))))) ;; 문자 스타일에 높이값이 정의가 되어있는지에 따라 텍스트가 만들어지는 로직이 달라짐
	(if (= hstyle 0.0)
		(setq h (* height 2))
		(setq h (* hstyle 2))
	)
	(setq w (* 2 (+ bw h)))
	;(setq w (* h 8))
	
	(setq pp1 (polar p1 0 w)) ;; 우측 상단 포인트
	(setq pp2 (polar pp1 (dtr 270) h)) ;; 우측 하단 포인트
	(setq pp3 (polar pp2 (dtr 180) w)) ;; 좌측 하단 포인트
	(setq pp4 (polar pp3 (dtr 90) h)) ;; 좌측 상단 포인트
	
	(setq lp1 (polar pp4 (dtr 0) (/ w 2))) ;; 좌측 상단 중단 포인트
	(setq lp2 (polar pp3 (dtr 0) (/ w 2))) ;; 좌측 하단 중단 포인트
	
	(setq tp1 (polar lp2 (dtr 45) (/ w 8))) ;; 오른쪽 문자
	(setq tp2 (polar pp3 (dtr 45) (/ w 8))) ;; 왼쪽 문자
	
	(setvar "cmdecho" 0)
	;; 테이블 상단 주석
	(command "pline" pp1 pp2 pp3 pp4 "c" "")
	(command "line" lp1 lp2 "")
	;(setq bname (cdr (assoc 2 (entget (car ent)))))
	(if (= hstyle 0.0)
		(command "text" tp2 height "" "레이어명" "")
		(command "text" tp2 "" "레이어명" "")
	)
	(setq l_ent1 (entlast))
	(if (= hstyle 0.0)
		(command "text" tp1 height "" "길이" "")
		(command "text" tp1 "" "길이" "")
	)
	(setq l_ent2 (entlast))
	
	(setq cen1 (polar pp3 (angle pp3 lp1) (/ (distance pp3 lp1) 2))) ;; 왼쪽 문자에 대한 사각형의 센터 포인트
	(setq cen2 (polar lp2 (angle lp2 pp1) (/ (distance lp2 pp1) 2))) ;; 오른쪽 문자에 대한 사각형의 센터 포인트
	
	(text_sort l_ent1 cen1)
	(text_sort l_ent2 cen2)
	
	(foreach lay laylist
		(setq entlist (car set_entlist))
		(setq tl 0)
		(repeat (length entlist)
			(setq ent (car entlist))
			(setq tl (+ tl (vlax-curve-getDistAtParam ent (vlax-curve-getEndParam ent))))
			(setq entlist (cdr entlist))
		)
		(setq pp1 (polar pp3 0 w)) ;; 우측 상단 포인트
		(setq pp2 (polar pp1 (dtr 270) h)) ;; 우측 하단 포인트
		(setq pp3 (polar pp2 (dtr 180) w)) ;; 좌측 하단 포인트
		(setq pp4 (polar pp3 (dtr 90) h)) ;; 좌측 상단 포인트
		
		(setq lp1 (polar pp4 (dtr 0) (/ w 2))) ;; 좌측 상단 중단 포인트
		(setq lp2 (polar pp3 (dtr 0) (/ w 2))) ;; 좌측 하단 중단 포인트
		
		(setq tp1 (polar lp2 (dtr 45) (/ w 8))) ;; 오른쪽 문자
		(setq tp2 (polar pp3 (dtr 45) (/ w 8))) ;; 왼쪽 문자
		
		(setvar "cmdecho" 0)
		(command "pline" pp1 pp2 pp3 pp4 "c" "")
		(command "line" lp1 lp2 "")
		
		(if (= hstyle 0.0)
			(command "text" tp2 height "" lay "")
			(command "text" tp2 "" lay "")
		)
		(setq l_ent1 (entlast))
		(setq tl (rtos (* (/ tl 1000) sc) 2 dpoint))
		(if (= hstyle 0.0)
			(command "text" tp1 height "" tl "")
			(command "text" tp1 "" tl "")
		)
		(setq l_ent2 (entlast))
		
		(setq cen1 (polar pp3 (angle pp3 lp1) (/ (distance pp3 lp1) 2))) ;; 왼쪽 문자에 대한 사각형의 센터 포인트
		(setq cen2 (polar lp2 (angle lp2 pp1) (/ (distance lp2 pp1) 2))) ;; 오른쪽 문자에 대한 사각형의 센터 포인트
		
		(text_sort l_ent1 cen1)
		(text_sort l_ent2 cen2)
		
		(setq set_entlist (cdr set_entlist))
	)	
	(command "undo" "end")
	(setvar "cmdecho" 1)
	(princ)
)






