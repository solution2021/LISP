(defun 객체테두리표시 (#aa / #a #b)
(vla-getboundingbox #aa '#a '#b)
(setq #a(vlax-safearray->list #a)) 
(setq #b(vlax-safearray->list #b))
(grvecs (list
-5 (list (car #a)(cadr #a))(list (car #b)(cadr #a))
-5 (list (car #b)(cadr #a))(list (car #b)(cadr #b)) 
-5 (list (car #b)(cadr #b))(list (car #a)(cadr #b))
-5 (list (car #a)(cadr #b))(list (car #a)(cadr #a))
);;list
);;grvecs
);;defun
;;========================================================================================
(defun 면적체적중량표시 (/ #ap #ad #ut #as #os #count #index #a #b #c #d #e #f #g #h #i)
(vl-load-com)
(setq #ap(vlax-get-acad-object))
(setq #ad(vla-get-activedocument #ap))
(setq #ut(vla-get-utility #ad))
(setq #as
(if (>(vla-get-activespace #ad)0)
(vla-get-modelspace #ad) 
(vla-get-paperspace #ad) 
);;if
);;setq
(setq #os(vlax-variant-value(vla-getvariable #ad "osmode")))
(vla-setvariable #ad "cmdecho" 0)
(vla-setvariable #ad "osmode" 0)
(setq #count (vla-get-count #as))
(while (not(vl-catch-all-error-p(setq #a(vl-catch-all-apply 'vla-getpoint (list #ut nil "\n≫≫≫Pick internal point:")))))
(if(not(vl-catch-all-error-p(vl-catch-all-apply 'vl-cmdf 
(list
"boundary"
"a"
"o"
"r"
"" 
(vlax-safearray->list(vlax-variant-value #a))
""
) 
)
)
)
(progn
(if (and
(> (vla-get-count #as) #count)
(eq "AcDbRegion" (vla-get-objectname(vla-item #as(1-(vla-get-count #as)))))
);;and 
(progn
(setq #index 1) 
(repeat (- (vla-get-count #as) #count) 
(setq #e(vla-item #as (- (vla-get-count #as) #index)))
(setq #b(vlax-variant-value(vla-get-centroid #e)))
(setq #c(vlax-make-safearray vlax-vbdouble '(0 . 2)))
(vlax-safearray-fill #c (list 
(vlax-safearray-get-element #b 0)
(vlax-safearray-get-element #b 1)
0.0
);;list
);;vlax-
(setq #d(cons(vla-get-area #e)#d))
(setq #i(cons #e #i))
(setq #index(1+ #index))
);;repeat 
(mapcar '객체테두리표시 #i) 
(mapcar 'vla-delete #i)
(setq #i nil) 
(vlax-release-object #e)
(setq #d(abs(apply '- #d))) 
;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
(initget 0 "S GL 304 316 A B")(setq #h(getkword "\n≫≫≫Meterial [Steel,CGL,Sus304,Sus316,Aluminum,Brass]?:"))
(setq #g 
(cond
((eq #h "S")'("Steel" 7.85))
((eq #h "GL")'("CGL" 8.05))
((eq #h "304")'("Sus304" 7.93))
((eq #h "316")'("Sus316" 7.98))
((eq #h "A")'("Aluminum" 2.7))
((eq #h "B")'("Brass" 8.65))
(t '("Steel" 7.85))
);;cond
);;setq
(vla-addmtext #as #c 0.0 (strcat "Area=" (rtos #d 2 2) "㎟" (chr 92) "P"
"Thickness=" (rtos(setq #f(getreal "\n≫≫≫Thickness?<㎜>:"))2 2) "㎜" (chr 92) "P"
"Cubic=" (rtos(* #d #f)2 2) "㎣" (chr 92) "P"
;"Weight={\\C1;" (rtos(/(* #d #f (cadr #g))1000000)2 2) "㎏}" (chr 92) "P"
"Weight={\\C1;" (rtos(/(* #d #f (cadr #g))1000000)2 2)  (chr 92) "P"
"Material={\\C5;" (car #g) "}" 
);;strcat
);;vla- 
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(setq #count(vla-get-count #as))
);;progn
(progn
(princ "\nNot internal point.≪≪≪")
);;progn
);;if
);;progn
);;if 
(setq #d nil) 
);;while 
(vla-setvariable #ad "cmdecho" 1)
(vla-setvariable #ad "osmode" #os)
(mapcar 'vlax-release-object (list #as #ut #ad #ap))
(princ "\n「Designed by nonno」")
(princ)
);;defun
;;=====================================
(defun c:ww ()(면적체적중량표시))
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;사용방법
;;선택집합의 (제일 큰닫힌객체 빼기 나머지 닫힌객체들) 을 연산하여
;;면적,체적,두께,중량을 엠텍스트로 그래픽에 씁니다.
