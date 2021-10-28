(defun c:aabb 

	( / os tol 
		
		_lst->str
		_objects
		_sortobj
		_PutClipboard
		
	)
	
	(defun _lst->str ( lst del )
		(if (cdr lst)
			(strcat (car lst) del (_lst->str (cdr lst) del))
			(car lst)
		)
	)
	
	(defun _objects ( ss / i re )
		(if ss
			(repeat (setq i (sslength ss))
				(setq re (cons (vlax-ename->vla-object (ssname ss (setq i (1- i)))) re))
			)
		)
	)
	
	(defun _sortobj 
		
		(	olst typ tol / 
			typ objs opt npt lst data lst rev sx sy dxf x y
			 _s1 _s2 _s3 _s4
		)
		
		(defun rev (ls f) (mapcar '(lambda (l)(if (setq f (not f)) (reverse l) l)) ls))
		(defun sx  (objs) (vl-sort objs '(lambda (a b) (< (x a) (x b)))))
		(defun sy  (objs) (vl-sort objs '(lambda (a b) (< (y a) (y b)))))
		(defun dxf (o c)  (cdr  (assoc c (entget (vlax-vla-object->ename o)))))
		(defun x   (o)    (car  (trans (dxf o 10) (dxf o 210) 0)))
		(defun y   (o)    (cadr (trans (dxf o 10) (dxf o 210) 0)))
		
		(setq typ (vl-string->list (strcase typ)))
		
		(if (member (car typ) '(76 82))
			(setq _s1 sy _s2 y _s3 sx _s4 rev)
			(setq _s1 sx _s2 x _s3 sy _s4 rev)
		)
		
		(setq objs (_s1 olst) opt (_s2 (car objs)))
		
		(foreach o objs
			(if (< tol (abs (- (setq npt (_s2 o)) opt)))
				(setq lst  (cons data lst) data (list o) opt npt)
				(setq data (cons o data))
			)
		)
		
		(setq lst (mapcar '(lambda (l) (_s3 l))(cons data lst))
			  lst (if (member (cadr typ) '(85 82)) (reverse lst) lst)
			  lst (if (member (car typ)  '(68 76)) (mapcar '(lambda (l) (reverse l)) lst) lst)
			  lst (if (/= (car typ) (caddr typ))(_s4 lst t) lst)
		)
	)
	
	(defun _PutClipboard ( s / o p d r)
		(if
			(and s
				(setq o (vlax-create-object "htmlfile"))
				(setq p (vlax-get o 'parentwindow))
				(setq d (vlax-get p 'clipboarddata))
			)
			(progn
				(vlax-invoke d 'cleardata "text")
				(vlax-invoke d 'setdata "text" s)
				(setq r (= (vlax-invoke d 'getdata "text") s))
			)
		)
		(mapcar 'vlax-release-object (list o p d))
		r
	)
	
	(if (setq os  (_Objects (setq qs (ssget "_:L" '((0 . "TEXT"))))))
		(progn
			(setq tol (* 0.8 (vla-get-height (car os)))
				  os  (_SortObj os "rdr" tol) ccc os
			)		
			(if (= (length (cadr os)) 1)
			    (_PutClipboard
			       (_lst->str
				    	(mapcar
					    	'(lambda ( oo )
							
						    	(_lst->str
							    	(mapcar
	    								'(lambda ( o )
		    								 (vla-get-textstring o)
		    							) oo
	    							) "\t"
	    						)
	    					) os
	    				) "\n"
	    			)
		    	 ) 			
	    		(_PutClipboard
	    		   (_lst->str
    				  (mapcar   			  
		    		     '(lambda (a)
	    				    (if (wcmatch a "*\t*")
    					       (substr a (+ (vl-string-position (ascii "\t") a) 2))
    					       " "
    					     )
                           )					 				
					      (mapcar
	    					 '(lambda ( oo )
		    					 (_lst->str
		    						(mapcar
		    							'(lambda ( o )
		    								 (vla-get-textstring o)
		    							  ) oo
		    						) "\t"
		     					 )
		    				  ) os
	    				  ) 
	     			   ) "\n"
	    			)
	    		  )
		    )		
			
			(mapcar '(lambda (a) (vla-put-color (car a) 1)) os)
			
			(princ (strcat "\n" (itoa (length os)) "개의 문자가 복사되었습니다. CTRL+V 를 사용해서 붙여넣으세요"))
			
			(princ)
		)
	)
)(vl-load-com)