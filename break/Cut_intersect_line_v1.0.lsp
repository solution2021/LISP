;;  ______.`' Cut intersect with line '`._________________________________
;;  Made By ...... TaeEun  -_-)=b
;;
;;  Contact me ... arin9916@naver.com
;;             ... http://cafe.naver.com/ptelisp
;;
;;  ver 1.0 ...... Designed & Created
;;
;;  Description .. 
;;
;;     1. 자를 라인 선택 -> 기준 라인 선택 -> cutting length 입력
;;
;;     2. 명령어 수정을 하실려면 맨위에 (defun c:xxx .... 
;;        에서 xxx위치의 문자를 변경하시면 됩니다.
;;  _____________________________________________________.`' 12/06/14 '`.

(defun c:t2 ( / os1 os2 OOv PTE:objects PTE:start PTE:end PTE:memoVar) ;#120613Gval
	
    ;-----------------------------------------------------------------------------
    ; Sub function
    ;-----------------------------------------------------------------------------
    (defun PTE:objects ( ss / i re )
        (if ss
            (repeat (setq i (sslength ss))
                (setq re (cons (vlax-ename->vla-object (ssname ss (setq i (1- i)))) re))
            )
        )
    )
	
    ;-----------------------------------------------------------------------------
    ; Sub function
    ;-----------------------------------------------------------------------------
	(defun PTE:start( lst ) 
		(vla-startundomark (PTE:end nil))
		(list lst (mapcar 'getvar lst))
	)
	
    ;-----------------------------------------------------------------------------
    ; Sub function
    ;-----------------------------------------------------------------------------
	(defun PTE:end ( d / doc )
		(setq doc (vla-get-activedocument (vlax-get-acad-object)))
		(and (cadr d) (mapcar 'setvar (car d) (cadr d)))
		(and (= 8 (logand 8 (getvar 'UNDOCTL))) (vla-endundomark doc)) doc
	)
	
    ;-----------------------------------------------------------------------------
    ; Sub function
    ;-----------------------------------------------------------------------------
	(defun PTE:memoVar ( va f m s / v )
		(setq v (if (eval va) (eval va) s))
		(mapcar 'princ (list "\n" m "<" v ">: "))
		(set va ( f ))
		(if (not (eval va)) (set va v)) (eval va) 
	)

	;;===========================================================================
	;;
	;;    ...... Main Function
	;;===========================================================================
	(setq os1 (PTE:objects (ssget '((0 . "line"))))
		  os2 (PTE:objects (ssget '((0 . "line"))))
		  OOv (PTE:start nil)
	)
	
	(PTE:memoVar '#120613Gval getdist "Cut length :" 1)
	
	(mapcar
		'(lambda ( o1 / L3 lst l o )
			(setq lst '(0.))
			(mapcar
				'(lambda ( o2 / l )
					(if (setq l
							(vlax-curve-getdistatpoint o1
								(vlax-invoke o1 'intersectwith o2 acextendnone)
							)
						)
						(setq lst (cons l lst))
					)
				) os2
			)
			(setq l   (vlax-curve-getdistatpoint o1 (vlax-curve-getendpoint o1)) 
				  lst (vl-sort (cons l lst) '<)
				  o   (vla-copy o1)
			)
			
			(if lst
				(mapcar
					'(lambda ( d1 d2 / L1 L2 )
					
						(setq L1 (- d2 #120613Gval)
							  L2 (+ d2 #120613Gval)
						)
						
						(if (and (/= d2 (last lst)) (< d1 L1))
							(vlax-put o 'endpoint (vlax-curve-getpointatdist o1 L1))
						)
						
						(if (> (last lst) L2)
							(progn
								(if (or (not L3) (< L3 L1)) (setq o (vla-copy o1)))
								(vlax-put o 'startpoint (vlax-curve-getpointatdist o1 L2))
							)
						)
						(setq L3 L2)
					) lst (cdr lst)
				)
			)
			(vla-delete o1)
		) os1
	) (PTE:end OOv)
	(princ)
)(vl-load-com)