(
;;------------------------------`' HOLE POINT '`--------------------------------;;
;;                                                                              ;;
;;  Description                                                                 ;;
;;    - 금형의 홀좌표 추출하는 리습입니다.                                      ;;
;;    - 가장 내부의 원, 아크를 지름으로 인식합니다.                             ;;
;;------------------------------------------------------------------------------;;
;;  Author : 														            ;;
;;	  - PTE LISP co.                                                            ;;
;;	  - arin9916@naver.com                                                      ;;
;;	  - http://cafe.naver.com/ptelisp                                           ;;
;;------------------------------------------------------------------------------;;
;;  Version 														            ;;
;;    - 1.0 : Design & Created                                       (12/09/26) ;;
;;    - 1.1 : 블럭이 겹칠경우 1개로 인식                             (12/10/12) ;;
;;    - 1.2 : 사용자 옵션추가 ( In  Arc  ArcB  Sort )                (12/10/23) ;;
;;            객체가 1개선택될 경우 출력이 안되던 문제점 수정                   ;;
;;            블럭내부객체 Sort문제 수정                                        ;;
;;    - 1.3 : 소수점 유효자리수 입력 추가                            (12/10/30) ;;
;;------------------------------------------------------------------------------;;
)
(defun c:ww

	( / l ll doc spc OOv cp objs hands blks ip num StNu st snu sc p s tsiz nu 
		In Arc ArcB Sort
		
;		*HolePointNum120925
;		*HolePointInt121030		
		_AddTxt
		_GetNo
		
		LM:nAssoc++
		
		PTE:Start
		PTE:End
		PTE:objects
		PTE:Doc
		PTE:Dp->Rp
		PTE:memoVar
		PTE:DivideNum
		PTE:sortobj
		
		*error*
		
	)
	
	;-------------------------------------------------------------------------
	; Sub Function
	;-------------------------------------------------------------------------
	(defun _AddTxt ( str p ar tsiz / o )
		(setq o (vlax-invoke spc 'addtext str p tsiz))
		(vla-put-alignment o ar)
		(vlax-put o 'TextAlignmentPoint p) o
	)
	
	;-------------------------------------------------------------------------
	; Sub Function
	;-------------------------------------------------------------------------
	(defun _GetNo ( str / data a b s n )
		
		(setq data (PTE:DivideNum str)
			  a    (car  data)
			  b    (cadr data)
		)
		
		(cond
			( (not b)        (setq s (if a (car a) "")  n 1))
			( (= (car b) 0)  (setq s (if (cadr a) (cadr a) "")  n (atoi (car  a))))
			( (= (car b) 1)  (setq s (if (car a)  (car a)  "")  n (atoi (cadr a))))
			( t (setq s "A" n 1))
		) (list s n)
	)
	
	;-------------------------------------------------------------------------
	; Sub Function
	;-------------------------------------------------------------------------
	(defun LM:nAssoc++ ( key lst / pair )
		(if key
			(if (setq pair (assoc (car key) lst))
				(subst (cons (car key) (LM:nAssoc++ (cdr key) (cdr pair))) pair lst)
				(cons  (cons (car key) (LM:nAssoc++ (cdr key) nil)) lst)
			)
			(if lst (list (1+ (car lst))) '(1))
		)
	)

	;-------------------------------------------------------------------------
	; Sub Function
	;-------------------------------------------------------------------------
	(defun PTE:start( lst ) 
		(vla-startundomark (PTE:end nil))
		(list lst (mapcar 'getvar lst))
	)

	;-------------------------------------------------------------------------
	; Sub Function
	;-------------------------------------------------------------------------
	(defun PTE:end ( d / doc )
		(setq doc (vla-get-activedocument (vlax-get-acad-object)))
		(and (cadr d) (mapcar 'setvar (car d) (cadr d)))
		(while (= 8 (logand 8 (getvar 'UNDOCTL))) (vla-endundomark doc)) doc
	)
	
	;-------------------------------------------------------------------------
	; Sub Function
	;-------------------------------------------------------------------------
	(defun PTE:objects ( ss / i re )
		(if ss
			(repeat (setq i (sslength ss))
				(setq re (cons (vlax-ename->vla-object (ssname ss (setq i (1- i)))) re))
			)
		)
	)
	
	;-------------------------------------------------------------------------
	; Sub Function
	;-------------------------------------------------------------------------
	(defun PTE:Doc ( ^doc ^spc / doc^ )
		(setq doc^ (vla-get-activedocument (vlax-get-acad-object)))
		(if ^spc
			(set ^spc
				(vlax-get-property doc^ 
					(if (= 1 (getvar 'CVPORT)) 'Paperspace 'Modelspace)
				)
			)
		) (if ^doc (set ^doc doc^))
	)
	
	;-------------------------------------------------------------------------
	; Sub Function
	;-------------------------------------------------------------------------
	(defun PTE:Dp->Rp ( bobj pt / Dpt Rpt ang _matrixCal )

		(defun _matrixCal ( b mt )
			(mapcar '(lambda ( m ) (apply '+ (mapcar '* m b))) mt)
		)
		
		(setq Dpt (cdr (assoc 10 (tblsearch "block" (vla-get-name bobj))))
			  Rpt (vlax-get bobj 'InsertionPoint)
			  ang (vlax-get bobj 'rotation)
		)
		(mapcar '+ Rpt
			(_matrixCal
				(mapcar '* 
					(mapcar '- pt Dpt)
					(list 
						(vlax-get bobj 'XscaleFactor)
						(vlax-get bobj 'YscaleFactor)
						(vlax-get bobj 'ZscaleFactor)
					)
				)
				(list
					(list (cos ang) (sin (- ang)) 0.)
					(list (sin ang) (cos    ang)  0.)
					(list        0.           0.  1.)
				)
			)
		)
	)
	
	;-------------------------------------------------------------------------
	; Sub Function
	;-------------------------------------------------------------------------
	(defun PTE:memoVar ( va f m s / v )
		(setq v (if (eval va) (eval va) s))
		(mapcar 'princ (list "\n" m " <" v "> : "))
		(set va ( f ))
		(if (member (eval va) '(nil "")) (set va v)) (eval va) 
	)
	
	;-------------------------------------------------------------------------
	; Sub Function
	;-------------------------------------------------------------------------
	(defun PTE:DivideNum ( str / lst s m v1 v2 i j c _NumP _Cal)

		(defun _NumP ( x ) (<= 48 x 57))
		(defun _Cal ( ty v )
			(set v (cons (vl-list->string (reverse (eval ty))) (eval v))) 
			(set ty nil)
		)
		
		(setq lst (vl-string->list str) i -1 j -1)
		
		(repeat (length lst)
			(setq c (nth (setq i (+ i 1)) lst))
			
			(cond
				(	(_NumP c) 
					(setq s (cons c s) ) (cond ( m (_Cal 'm 'v1) (setq j (+ 1 j)))))
				
				(	(and (= c 46) (> i 0) (_NumP (nth (- i 1) lst)) (_NumP (nth (+ i 1) lst)))
					(setq s (cons c s))
				)
				(t  (setq m (cons c m))
					(cond ( s (_Cal 's 'v1) (setq v2 (cons (setq j (+ 1 j)) v2))))
				)
			)
		)
		(cond
			( m (_Cal 'm 'v1))
			( t (_Cal 's 'v1) (setq v2 (cons (setq j (+ 1 j)) v2)))
		) (list (reverse v1) (reverse v2))
	)

	;-------------------------------------------------------------------------
	; Sub Function
	;-------------------------------------------------------------------------	
    (defun PTE:sortobj
    
        (   olst typ tol /
            typ objs opt npt lst data lst rev sx sy dxf x y
            PTE:s1 PTE:s2 PTE:s3 PTE:s4
        )
        
        (defun rev (ls f) (mapcar '(lambda (l)(if (setq f (not f)) (reverse l) l)) ls))
        (defun sx  (objs) (vl-sort objs '(lambda (a b) (< (x a) (x b)))))
        (defun sy  (objs) (vl-sort objs '(lambda (a b) (< (y a) (y b)))))
        (defun dxf (o c)  (cdr  (assoc c (entget (vlax-vla-object->ename o)))))
        (defun x   (o)    (car  (trans (dxf o 10) (dxf o 210) 0)))
        (defun y   (o)    (cadr (trans (dxf o 10) (dxf o 210) 0)))
        
        (setq typ (vl-string->list (strcase typ)))
        
        (if (member (car typ) '(76 82)) ; R or L
            (setq PTE:s1 sy PTE:s2 y PTE:s3 sx PTE:s4 rev)
            (setq PTE:s1 sx PTE:s2 x PTE:s3 sy PTE:s4 rev)
        )
        
        (setq objs (PTE:s1 olst) opt (PTE:s2 (car objs)))
        (foreach o objs
            (if (< tol (abs (- (setq npt (PTE:s2 o)) opt)))
                (setq lst  (cons data lst) data (list o) opt npt)
                (setq data (cons o data))
            )
        )
        
        (setq lst (mapcar '(lambda (l) (PTE:s3 l))(cons data lst))
              lst (if (member (cadr typ) '(85 82)) (reverse lst) lst) ; U or R
              lst (if (member (car typ)  '(68 76)) (mapcar '(lambda (l) (reverse l)) lst) lst) ; D or L
              lst (if (/= (car typ) (caddr typ))(PTE:s4 lst t) lst)
        )
    )
	
	;-------------------------------------------------------------------------
	; Error Function
	;-------------------------------------------------------------------------
	(defun *error* (s) (if OOv (PTE:End)) (princ s))

	
	;-----------------------------------------------------------------------------------------------
	; Main Function
	;-----------------------------------------------------------------------------------------------
	(PTE:Doc 'doc 'spc)
	
	(setq In   t     ; t   : 가장 내부객체를 지름으로 할 경우
					 ; nil : 외부객체를 지름으로 할 경우
					 
		  ArcB t     ; t   : 블럭내부의 아크객체를 고려
					 ; nil : 블럭내부의 아크객체를 무시
					 
		  Arc  t     ; t   : 아크객체 선택가능
					 ; nil : 아크객체 선택불가능
					 
		  sort "uru" ; 같은 지름의 객체들의 정렬방식
                     ; rdr : Right -> Down  -> Right
                     ; ldl : Left  -> Down  -> Left				 
                     ; rur : Right -> Up    -> Right		 
                     ; lul : Left  -> Up    -> Left
                     ; drd : Down  -> Right -> Down
                     ; uru : Up    -> Right -> Up 
                     ; dld : Down  -> Left  -> Down
                     ; ulu : Up    -> Left  -> Up
                     ; rdl : Right -> Down  -> Left
                     ; ldr : Left  -> Down  -> Right
                     ; rul : Right -> Up    -> Left
                     ; lur : Left  -> Up    -> Right
                     ; dru : Down  -> Right -> Up
                     ; urd : Up    -> Right -> Down
                     ; dlu : Down  -> Left  -> Up
                     ; uld : Up    -> Left  -> Down
	)
	
	

	(vlax-put (vlax-invoke (vla-get-layers doc) 'add "HOLEPOINT") 'color 2)
	
	(setq OOv   (PTE:start '(DIMZIN CLAYER CECOLOR DIMSCALE))
		  cp    (trans (getpoint "\nCenter point :") 1 0)
		  objs  (PTE:objects (if Arc (ssget '((0 . "insert,circle,arc"))) (ssget '((0 . "insert,circle")))))
		  objs  (apply 'append (PTE:SortObj objs sort 0.001))
		  
		  blks  (vla-get-blocks doc)
		  ip    (trans (getpoint "\nTable point :") 1 0)
		  num   (PTE:memoVar '*HolePointNum120925 getstring "Start Number" "A1")
		  StNu  (_GetNo num)
		  st    (car  StNu)
		  snu   (cadr StNu)
		  sc    (getvar 'DIMSCALE)
		  nu    (PTE:memoVar '*HolePointInt121030 getint "소수점 유효자리수" 2)
	)
	
	(if (> (length objs) 1)
		(setq objs
			(vl-remove nil
				(mapcar
					'(lambda ( o1 o2 )
						(if
							(and
								(= (vla-get-objectname o1) "AcDbBlockReference")
								(= (vla-get-objectname o2) "AcDbBlockReference")
								(= (vla-get-name o1) (vla-get-name o2))
								(equal (vlax-get o1 'InsertionPoint) (vlax-get o2 'InsertionPoint) 0.001)
							) nil o1
						)
					) objs (append (cdr objs) (list (car objs)))
				)
			)
		)
	)
	
	(setq hands (mapcar 'vla-get-handle objs))
	
	(setvar 'CLAYER "HOLEPOINT")
	(setvar 'CECOLOR "256")
	(setvar 'DIMZIN 0)
	(if (= (getvar 'DIMSCALE) 0) (setvar 'DIMSCALE 1))
	
	(mapcar
		'(lambda ( o / ps rs n h p x y m s ) ; l ll
			
			(setq n (vla-get-objectname o)
				  h (vla-get-handle o)
			)
			
			(cond
				(	(member n '("AcDbCircle" "AcDbArc"))
					(setq p (mapcar '- (vlax-get o 'center) cp)
						  x (atof (rtos (car  p) 2 3))
						  y (atof (rtos (cadr p) 2 3))
						  l (LM:nAssoc++ (list x y h) l)
					)
				)
				(	t
					(vlax-for Bo (vla-item blks (vla-get-name o))
						(if (member (vla-get-objectname Bo) (if ArcB '("AcDbCircle" "AcDbArc") '("AcDbCircle")))
							(progn
								(setq ps (cons (vlax-get Bo 'center) ps)
									  rs (cons (vlax-get Bo 'radius) rs)
								)
							)
						)
					)
					
					
					(setq p  (mapcar '- (PTE:Dp->Rp o (car ps)) cp)
						  s  (* 2 (apply 'min rs) (abs (vla-get-XScaleFactor o)))
						  m  (* 2 (apply 'max rs) (abs (vla-get-XScaleFactor o)))
						  ll (cons (if In (list s o p m) (list m o p m)) ll)
					)
				)
			)
		) objs
	)
	


	(mapcar
		'(lambda ( Lv1 )
			(mapcar
				'(lambda ( Lv2 / data s m o p )
					(setq data
						(vl-sort
							(mapcar
								'(lambda ( Lv3 )
									(vlax-ename->vla-object (handent (car Lv3)))
								) (cdr Lv2)
							)
							'(lambda ( o1 o2 )
								(< (vla-get-radius o1) (vla-get-radius o2))
							)
						)
					)

					(setq s  (* 2. (vla-get-radius (car  data)))
						  m  (* 2. (vla-get-radius (last data)))
						  o  ((if In car last) data)
						  p  (mapcar '- (vlax-get o 'center) cp)
						  ll (cons (if In (list s o p m) (list m o p m)) ll)
					)
				) (cdr Lv1)
			)
		)l
	)
	
  (setq aa ll bb l)
	
  (setq ll
         (vl-sort
           (vl-sort ll
                    '(lambda ( o1 o2 )
                       (<  (vl-position (vla-get-handle (cadr o1)) hands) 
                           (vl-position (vla-get-handle (cadr o2)) hands))
                     )
           ) 
           '(lambda ( o1 o2 ) (< (car o1) (car o2)))
         )
  )
	
  ;수정
  (setq ##th(getvar 'TEXTSIZE))
  (setq th(getreal(strcat "\n문자크기 입력<"(rtos ##th 2 1)">:")))
  (if (= nil th)(setq th ##th))
  (setq offset (/ th 2.5))

  (setvar 'CLAYER "0")
  (setvar 'CECOLOR "3")
	
  (setq p    (polar ip (* pi 1.5) (* sc (* 2.5 offset)))
        s    0.
        ;tsiz (* sc 2.5))
        tsiz (* sc th))
	
  (setvar 'CLAYER "HOLEPOINT")
  (setvar 'CECOLOR "3")
	
  ;|
  (_AddTxt "NO"   (polar p 0. (* sc  7.5)) acAlignmentMiddleCenter tsiz)
  (_AddTxt "X"    (polar p 0. (* sc 25.0)) acAlignmentMiddleCenter tsiz)
  (_AddTxt "Y"    (polar p 0. (* sc 45.0)) acAlignmentMiddleCenter tsiz)
  (_AddTxt "%%cd" (polar p 0. (* sc 65.0)) acAlignmentMiddleCenter tsiz)
  |;
  
  (_AddTxt "NO"   (polar p 0. (* sc  (* 7.5  offset))) acAlignmentMiddleCenter tsiz)
  (_AddTxt "X"    (polar p 0. (* sc  (* 25.0 offset))) acAlignmentMiddleCenter tsiz)
  (_AddTxt "Y"    (polar p 0. (* sc  (* 45.0 offset))) acAlignmentMiddleCenter tsiz)
  (_AddTxt "%%cd" (polar p 0. (* sc  (* 65.0 offset))) acAlignmentMiddleCenter tsiz)
	
  (setq p (polar p (* pi 1.5) (* sc (* 2.5 offset))))
	
  (mapcar
    '(lambda ( data / f tp )
		
       (if (null (equal s (car data) 0.0001))
         (setq s (car data) f t)
       )
			
       (setvar 'CLAYER "0") 
       (setvar 'CECOLOR "3")
			
       ;(vlax-invoke spc 'addline p (polar p 0. (* sc (if f 75. 55.))))
       (vlax-invoke spc 'addline p (polar p 0. (* sc (if f (* 75. offset) (* 55. offset)))))
		;|
		(setq p   (polar p (* pi 1.5) (* sc 2.5))
		tp  (polar (mapcar '+ (caddr data)  cp) (* pi 0.25) (* 0.5 (cadddr data)))
		)
		|;
       (setq p   (polar p (* pi 1.5) (* sc (* 2.5 offset)))
             tp  (polar (mapcar '+ (caddr data)  cp) (* pi 0.25) (* 0.5 (cadddr data)))
       )

       (setvar 'CLAYER "HOLEPOINT") 
       (setvar 'CECOLOR "256")
			
       (_AddTxt 
         (strcat st (itoa snu))          
         (polar p 0. (* sc  (* 7.5 offset))) 
         acAlignmentMiddleCenter 
         tsiz
       )
       
       (_AddTxt 
         (rtos (caaddr data) 2 nu)       
         (polar p 0. (* sc (* 33.0 offset))) 
         acAlignmentMiddleRight  
         tsiz
       )
			
       (_AddTxt 
         (rtos (cadr (caddr data)) 2 nu) 
         (polar p 0. (* sc (* 53.0 offset))) 
         acAlignmentMiddleRight  
         tsiz
       )
			
       (_AddTxt 
         (strcat st (itoa snu))          
         tp    
         acAlignmentBottomLeft   
         tsiz
       )
			
       (if f 
         (_AddTxt 
           (strcat "%%c" (rtos (car data) 2 nu)) 
           (polar p 0. (* sc (* 73.0 offset))) 
           acAlignmentMiddleRight  
           tsiz
         )
       )
		
       (setq p   (polar p (* pi 1.5) (* sc (* 2.5 offset)))
             snu (+ snu 1)
       )
     ) ll
	)
	
	(setvar 'CLAYER "0")
	(setvar 'CECOLOR "3")
	
	(mapcar
		'(lambda ( pt )
			(vlax-invoke spc 'addline (car pt) (cadr pt))
		)
		(list
			(list ip                         p                      )
			(list p                         (polar p 0. (* sc  (* 75. offset))))
			(list ip                        (polar ip 0.(* sc  (* 75. offset))))
			(list (polar ip 0. (* sc  (* 15. offset))) (polar p 0. (* sc  (* 15. offset))))
			(list (polar ip 0. (* sc  (* 35. offset))) (polar p 0. (* sc  (* 35. offset))))
			(list (polar ip 0. (* sc  (* 55. offset))) (polar p 0. (* sc  (* 55. offset))))
			(list (polar ip 0. (* sc  (* 75. offset))) (polar p 0. (* sc  (* 75. offset))))
		)
	)
	(PTE:End OOv)
	(princ)
)(vl-load-com)