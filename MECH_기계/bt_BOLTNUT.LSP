;;*************************************************************************
;;* BOLT/NUT/TAP/STOP BOLT/HINGE BOLT/SLOT HOLE/EYE BOLT/HANDWHEEL        *
;;* 1999/6/3                                            By K.W.Park       *
;;*************************************************************************
(defun bot_er (s)
(if (/= s "Function Cancelled")
(if (= s "quit / exit abort")
 (princ)
 (princ (strcat "\nERROR :" s))
)
)
(setvar "cmdecho" ocmde)                               
(setvar "osmode" oosmo)
(setvar "dimscale" ods)
(setvar "peditaccept" oed)  
(graphscr)
(setq *error* olderr)
(princ)
)
;--------------------
(defun dtr (dtr1)
(* pi (/ dtr1 180.00))
)
(defun rtd (rtd1)
(/ (* 180.00 rtd1) pi)
)
(defun tan (tn)
(/ (sin tn) (cos tn))
)
;----------------------
(defun tplist ()
(cond
((= an2 2)  (setq size '( 2 4 3.8 0.4))) 
((= an2 2.2)(setq size '( 2.2 4.5 4.3 0.45))) 
((= an2 2.3)(setq size '( 2.3 4.5 4.3 0.4))) 
((= an2 2.5)(setq size '( 2.5 5 4.7 0.45))) 
((= an2 2.6)(setq size '( 2.6 5 4.7 0.45))) 
((= an2 3)  (setq size '( 3 5.5 5.3 0.6)))
((= an2 3.5)(setq size '( 3 6 5.8 0.6)))
((= an2 4)  (setq size '( 4 7 6.8 0.75)))
((= an2 4.5)(setq size '( 4.5 8 7.8 0.75)))
((= an2 5)  (setq size '( 5 8 7.8 0.9)))
((= an2 6)  (setq size '( 6 10 9.8 1)))
((= an2 7)  (setq size '( 7 11 10.8 1)))
((= an2 8)  (setq size '( 8 13 12.5 1.25)))
((= an2 10) (setq size '( 10 17 16.5 1.5)))
((= an2 12) (setq size '( 12 19 18 1.75)))
((= an2 14) (setq size '( 14 22 21 2)))
((= an2 16) (setq size '( 16 24 23 2)))
((= an2 18) (setq size '( 18 27 26 2.5)))
((= an2 20) (setq size '( 20 30 29 2.5)))
((= an2 22) (setq size '( 22 32 31 2.5)))
((= an2 24) (setq size '( 24 36 34 3)))
((= an2 27) (setq size '( 27 41 39 3)))
((= an2 30) (setq size '( 30 46 44 3.5)))
((= an2 33) (setq size '( 33 50 48 3.5)))
((= an2 36) (setq size '( 36 55 53 4)))
((= an2 39) (setq size '( 39 60 57 0)))
((= an2 42) (setq size '( 42 65 62 4.5)))
((= an2 45) (setq size '( 45 70 67 0)))
((= an2 48) (setq size '( 48 75 72 5)))
((= an2 52) (setq size '( 52 80 77 0)))
((= an2 56) (setq size '( 56 85 82 0)))
((= an2 60) (setq size '( 60 90 87 0)))
((= an2 64) (setq size '( 64 95 92 0)))
((= an2 68) (setq size '( 68 100 97 0))) 
((= an2 72) (setq size '( 72 105 102 0)))
((= an2 76) (setq size '( 76 110 107 0)))
((= an2 80) (setq size '( 80 115 112 0))) 
((= an2 85) (setq size '( 85 120 116 0)))
((= an2 90) (setq size '( 90 130 126 0)))
((= an2 95) (setq size '( 95 135 131 0)))
((= an2 100) (setq size '( 100 145 141 0)))
((= an2 105) (setq size '( 105 150 146 0)))
((= an2 110) (setq size '( 110 155 151 0)))
((= an2 115) (setq size '( 115 165 161 0))) 
((= an2 120) (setq size '( 120 170 166 0)))
((= an2 125) (setq size '( 125 180 176 0)))
((= an2 130) (setq size '( 130 185 181 0))) 
) ;cond
)  
;----------------------------------------------------------------
(defun C:BT   (/ an1 an2 ph bl size d1 h d k ocmde oosmo ods gd
	         hc e m dp dk1 sl lh dp r h1 pt p1 p2 p3 p4 oed
	         p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 an
	         p18 p19 p20 p21 p22 p23 p24 p25 p1a p2a p3a p3b
	         p4u p7u p8b m1 m2 tad drd l1 l2 l3 cent cpt pt1
	         e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 e11 e12 e13 e14 ts
	         e15 e16 e17 e18 e19 e20 olderr pit ans r n1 n2 sf
	         d2 d1 w l c a1 a2 a p15-1 p16-1 tb1 odim ods sa
	         pt2 rad len ang kk lenn s1 s2 s3 s4 s5 h2 n wt1 wf
                 tbl s6 k1 k2 k3 k4 k5 k6 dia x1 x2 h1 wt2 dg
            	 ar1 ar2 c1 c2 ful oblip dh a lh db f lb x dc cgd
	         s6 s7 s8 s9 s10 s11 s12 s13 s14 the w nd do twt
	         di lt lf tn the gh t1 t2 dl eg1 ep1 eg2 ep2 tbl
	         hd hh bh tk an)
(command "undo" "be")    
(setq olderr *error* *error* bot_er)
(setq ocmde (getvar "cmdecho"))
(setq oosmo (getvar "osmode"))
(setq ods (getvar "dimscale"))
(setq oed (getvar "peditaccept"))
(setvar "peditaccept" 0)   
(setvar "cmdecho" 0)
(setvar "osmode" 0)
(setq tb1 (tblsearch "LAYER" "cen-m"))
  (if (= tb1 nil)
   (command "layer" "n" "cen-m" "c" 6 "cen-m" "lt" "center2" "cen-m" "")
)  
;---------------------------------------------------------------
(textpage)
(princ "\n========================================================")  
(princ "\n                 [ BOLT/NUT/TAP ]            <metric>")
(princ "\n========================================================")
(princ "\n    * Hex.Bolt---------(A)Elev.(+인장하중)      (B)Plan   ")
(princ "\n    * Wrench Bolt------(C)Elev.(+인장하중)      (D)Plan   ")
(princ "\n    * Wrench Bolt Hole-(CC)Elev.                  ")  
(princ "\n    * Hex.Nut----------(E)Elev.1종 (F)Elev.2종  (G) Plan  ")
(princ "\n    * Tap(M)-----------(H)Depth    (I)Thru.     (J) Plan  ")
(princ "\n    * Stop Bolt--------(K)Elev.                           ")
(princ "\n    * Hinge Bolt(경첩)-(L)Plan                     ")
(princ "\n    * Hand Wheel-------(M)Elev.    (N)plan         ")   
(princ "\n    * Eye Bolt---------(O)Plan                     ")
(princ "\n    * Slot Hole--------(P)Plan                     ")  
(princ "\n    * Hand Grip(R.B)---(Q)Elev.    (R)plan         ")
(princ "\n    * PT TAP(femail)---(S)Elev.    (T)plan         ")
(princ "\n    * Plain & Spring Washer--------(U)spec'        ")
(princ "\n========================================================")
(initget 7)
(setq an1 (strcase (getstring "\nSelect Alphabet: " )))
;------------ Hex.bolt --elevation
(if (= an1 "A")   
(progn
(prompt "\nBolt Range: 3,3.5,4,4.5,5,6,7,8,10,12,14,16,(18),20,(22),24,27,30,33,36,39,42,45,48,52,56,60,64,68,72,76,80")  
(initget 7)
(setq an2 (getreal "\nBolt diameter (mm) : " )) 
(cond
((= an2 3)  (setq size '(3 2 5.5 5.3 0.6 12 0 0 0.6)))
((= an2 3.5)(setq size '(3.5 2.4 6 5.8 0.6 14 0 0 0.6)))
((= an2 4)  (setq size '(4 2.8 7 6.8 0.8 14 0 0 0.75)))
((= an2 4.5)(setq size '(4.5 3.2 8 7.8 0.8 16 0 0 0.75)))
((= an2 5)  (setq size '(5 3.5 8 7.8 0.9 16 0 0 0.9)))
((= an2 6)  (setq size '(6 4 10 9.8 1 18 0 0 1)))
((= an2 7)  (setq size '(7 5 11 10.7 1 20 0 1)))
((= an2 8)  (setq size '(8 5.5 13 12.6 1.2 22 0 0 1.25)))
((= an2 10) (setq size '(10 7 17 16.5 1.5 26 0 0 1.5)))
((= an2 12) (setq size '(12 8 19 18 2 30 36 0 1.75)))
((= an2 14) (setq size '(14 9 22 21 2 34 40 0 2)))
((= an2 16) (setq size '(16 10 24 23 2 38 44 0 2)))
((= an2 18) (setq size '(18 12 27 26 2.5 42 48 0 2.5)))
((= an2 20) (setq size '(20 13 30 29 2.5 46 52 0 2.5)))
((= an2 22) (setq size '(22 14 32 31 2.5 50 56 0 2.5)))
((= an2 24) (setq size '(24 15 36 34 3 54 60 0 3)))
((= an2 27) (setq size '(27 17 41 39 3 60 66 79 3)))
((= an2 30) (setq size '(30 19 46 44 3.5 66 72 85 3.5)))
((= an2 33) (setq size '(33 21 50 48 3.5 72 78 91 3.5)))
((= an2 36) (setq size '(36 23 55 53 4 78 84 97 4)))
((= an2 39) (setq size '(39 25 60 57 4 84 90 103 0)))
((= an2 42) (setq size '(42 26 65 62 4.5 90 96 109 4.5)))
((= an2 45) (setq size '(45 28 70 67 4.5 96 102 115 0)))
((= an2 48) (setq size '(48 30 75 72 5 102 108 121 5)))
((= an2 52) (setq size '(52 33 80 77 5 0 116 129 0)))
((= an2 56) (setq size '(56 35 85 82 5.5 0 124 137 0)))
((= an2 60) (setq size '(60 38 90 87 5.5 0 132 145 0)))
((= an2 64) (setq size '(64 40 95 92 6 0 140 153 0)))
((= an2 68) (setq size '(68 43 100 97 6 0 148 161 0))) 
((= an2 72) (setq size '(72 45 105 102 6 0 156 169 0)))
((= an2 76) (setq size '(76 48 110 107 6 0 164 177 0)))
((= an2 80) (setq size '(80 50 115 112 6 0 172 185 0))) 
) ;cond
(setq pit (nth 8 size))
(setq pit (rtos pit))
(setq ph (getstring (strcat "\nPitch (mm) <" pit ">:" )))
 (if (= ph "")
  (setq ph (atof pit))
  (setq ph (atof ph)) 
 )  
(initget 7)
(setq bl (getreal "\nBolt length (mm) : " ))
(setq d1 (nth 0 size)
       h (nth 1 size)
       b (nth 2 size)
       d (nth 3 size)
       k (nth 4 size)      
      gd (- d1 (* 1.082532 ph))                 ;;gol dia.
      dg (* (/ (/ b 2.0) (cos (dtr 30.0))) 2.0) ;;diagonal dia.
      hc (/ (/ (- dg d) 2.0) (sin (dtr 60.0)))  ;;head chamber length
      dp (/ (- d1 gd) 2.0)                      ;;gol depth
       r (* d1 1.5)
      h1 (* hc (sin (dtr 30.0))))               ;;r height
(graphscr)
(initget 1)
(setvar "osmode" oosmo)
(setq pt (getpoint "\nPick head lower center : " ))
(setvar "osmode" 0)
(setq p1 (polar pt (dtr 0.0) (/ d1 2.0))
      p2 (polar pt (dtr 0.0) (/ dg 2.0))
     p3a (polar p2 (dtr 90.0) h)
      p4 (polar p3a (dtr 180.0) (/ (- dg d) 2.0))
     p3b (polar p4 (dtr -30.0) hc) 
      p5 (polar p1 (dtr 90.0) (- h h1))
      p6 (polar p5 (dtr 180.0) d1)
      p7 (polar p4 (dtr 180.0) d)
     p8b (polar p3b (dtr 180.0) dg)  
      p9 (polar p2 (dtr 180.0) dg)
     p10 (polar p1 (dtr 180.0) d1)
     p11 (polar p10 (dtr 270.0) bl)
     p12 (polar p11 (dtr 0.0) d1)
     p13 (polar pt (dtr 180.0) (/ gd 2.0))
     p14 (polar p11 (dtr 0.0) k)
     p15 (polar p12 (dtr 180.0) k)
     p16 (polar p13 (dtr 0.0) gd)
     p17 (polar p11 (dtr 90.0) k)
     p18 (polar p12 (dtr 90.0) k)
     p19 (polar pt (dtr 90.0) h)
     p20 (polar p13 (dtr 270.0) (- bl (- k dp))) 
     p21 (polar p16 (dtr 270.0) (- bl (- k dp)))
     p22 (polar p4 (dtr 180.0) 1.0)
     p23 (polar p4 (dtr -30.0) (/ hc 2.0)))
(command "line" p2 p3b "")                       (setq e1 (entlast))
(command "line" p3b p4 "")                       (setq e2 (entlast))
(command "line" p7 p4 "")                        (setq e3 (entlast))
(command "line" p6 p10 "")                       (setq e4 (entlast))
(command "line" p5 p1 "")                        (setq e5 (entlast))
(command "arc" p5 p19 p6)                        (setq e8 (entlast))
(command "circle" "3p" p5 "_tan" p22 "_tan" p23) (setq e6 (entlast))
(setq cent (entget e6))
(setq cpt (cdr (assoc 10 cent))) ;cen point
(setq crd (cdr (assoc 40 cent))) ;radius
(setq pt1 (polar cpt (dtr 60.0) crd))
(command "erase" e6 "")
(command "arc" pt1 "ce" cpt p5)                  (setq e7 (entlast))
(command "line" p18 p15 "")                      (setq e19 (entlast))
(command "mirror" e7 "" p19 pt "")               (setq e9 (entlast))
(command "mirror" e1 "" p19 pt "")               (setq e18 (entlast))
(command "mirror" e19 "" p19 pt "")              (setq e20 (entlast))
(command "line" p7 p8b "")                       (setq e10 (entlast))
(command "line" p9 p2 "")                        (setq e11 (entlast))
(command "line" p10 p17 "")                      (setq e12 (entlast))
(command "line" p14 p15 "")                      (setq e13 (entlast))
(command "line" p18 p1 "")                       (setq e14 (entlast))
(command "line" p17 p18 "")                      (setq e15 (entlast))
(command "line" p13 p20 "")                      (setq e16 (entlast))            
(command "line" p16 p21 "")                      (setq e17 (entlast))
(command "rotate" e1 e2 e3 e4 e5 e7 e8 e9 e10 e11 e12 e13 e14 e15 e16 e17 e18 e19 e20 "" pt "\\")
;
(setq an (strcase (getstring "\nCalculate The Strength? <Y/N> <Y>: ")))
 (if (or (= an "Y")(= an ""))
  (progn
   (textpage) 
   (princ (strcat "\n▼최소 인장하중 (N/mm^2)==================="
		  "\n KS B 0233(탄소강)↓"
                  "\n-------------------------------------------"
		  "\n4T:392   6T:588   7T:686   10T:981"
		  "\n-------------------------------------------"		  
		  "\n ISO 898-1(탄소강)↓"
                  "\n-------------------------------------------"
                  "\n3.6:330  4.6:400  4.8:420  5.6:500  5.8:520"
		  "\n6.8:600  8.8(d<16):800     8.8(d>16):830"
		  "\n9.8:900  10.9:1040  12.9:1220"
		  "\n-------------------------------------------"		  
		  "\n ISO 898-1(스테인레스/오스테나이트)↓"
                  "\n-------------------------------------------"
                  "\nA1:500   A2,A3:700   A4,A5:800"
		  "\n==========================================="))
     (setq ts (getreal "\n최소 인장하중/위표값 (N/mm^2): ")
           sf (getreal "\n안전율 (최소인장하중에 대한): ")
    	   sa (/ (/ ts sf) 9.8)	;(kg/mm^2)
	   wf (* (/ pi 4.0) (expt (/ gd d1) 2.0) (expt d1 2.0) sa));:d1=외경/gd=골지름/sa=허용하중(kg/mm^2)
     (graphscr)
  )   
 )  
(setq wt1 (/ (* 0.8660254038 (expt dg 2.0) h 7.85) 1000000.0));head wt
(setq cgd (+ gd dp))
(setq a (/ (* pi (expt cgd 2.0)) 4.0))
(setq wt2 (/ (* a bl 7.85) 1000000.0));stud wt
(setq twt (+ wt1 wt2))
(princ (strcat "\nYou select: M" (rtos d1) "x" (rtos bl) "xP" (rtos ph) "  중량(kg)≒" (rtos twt) "(@비중=7.85)"))
 (if (/= wf nil) (princ (strcat "  허용 인장하중(kgf)= " (rtos wf 2 2))))
) ;progn
) ;if
;------------ Hex.Bolt --top
(if (= an1 "B")  
(progn
(prompt "\nBolt Range: 3,3.5,4,4.5,5,6,7,8,10,12,14,16,(18),20,(22),24,27,30,33,36,39,42,45,48,52,56,60,64,68,72,76,80")   
(initget 7)
(setq an2 (getreal "\nBolt diameter (mm) : " )) 
(graphscr) 
(initget 1)
(setvar "osmode" oosmo)
(setq pt (getpoint "\nPick head center : " ))
(setvar "osmode" 0)
(cond
((= an2 3)  (command "circle" pt "d" 5.3 "polygon" "6" pt "c" (/ 5.5 2.0)))
((= an2 3.5)(command "circle" pt "d" 5.8 "polygon" "6" pt "c" (/ 6.0 2.0)))
((= an2 4)  (command "circle" pt "d" 6.8 "polygon" "6" pt "c" (/ 7.0 2.0)))
((= an2 4.5)(command "circle" pt "d" 7.8 "polygon" "6" pt "c" (/ 8.0 2.0)))
((= an2 5)  (command "circle" pt "d" 7.8 "polygon" "6" pt "c" (/ 8.0 2.0)))
((= an2 6)  (command "circle" pt "d" 9.8 "polygon" "6" pt "c" (/ 10 2.0)))
((= an2 7)  (command "circle" pt "d" 10.7 "polygon" "6" pt "c" (/ 11 2.0)))
((= an2 8)  (command "circle" pt "d" 12.6 "polygon" "6" pt "c" (/ 13 2.0)))
((= an2 10) (command "circle" pt "d" 16.5 "polygon" "6" pt "c" (/ 17 2.0)))
((= an2 12) (command "circle" pt "d" 18 "polygon" "6" pt "c" (/ 19 2.0)))
((= an2 14) (command "circle" pt "d" 21 "polygon" "6" pt "c" (/ 22 2.0)))
((= an2 16) (command "circle" pt "d" 23 "polygon" "6" pt "c" (/ 24 2.0)))
((= an2 18) (command "circle" pt "d" 26 "polygon" "6" pt "c" (/ 27 2.0)))
((= an2 20) (command "circle" pt "d" 29 "polygon" "6" pt "c" (/ 30 2.0)))
((= an2 22) (command "circle" pt "d" 31 "polygon" "6" pt "c" (/ 32 2.0)))
((= an2 24) (command "circle" pt "d" 34 "polygon" "6" pt "c" (/ 36 2.0)))
((= an2 27) (command "circle" pt "d" 39 "polygon" "6" pt "c" (/ 41 2.0)))
((= an2 30) (command "circle" pt "d" 44 "polygon" "6" pt "c" (/ 46 2.0)))
((= an2 33) (command "circle" pt "d" 48 "polygon" "6" pt "c" (/ 50 2.0)))
((= an2 36) (command "circle" pt "d" 53 "polygon" "6" pt "c" (/ 55 2.0)))
((= an2 39) (command "circle" pt "d" 57 "polygon" "6" pt "c" (/ 60 2.0)))
((= an2 42) (command "circle" pt "d" 62 "polygon" "6" pt "c" (/ 65 2.0)))
((= an2 45) (command "circle" pt "d" 67 "polygon" "6" pt "c" (/ 70 2.0)))
((= an2 48) (command "circle" pt "d" 72 "polygon" "6" pt "c" (/ 75 2.0)))
((= an2 52) (command "circle" pt "d" 77 "polygon" "6" pt "c" (/ 80 2.0)))
((= an2 56) (command "circle" pt "d" 82 "polygon" "6" pt "c" (/ 85 2.0)))
((= an2 60) (command "circle" pt "d" 87 "polygon" "6" pt "c" (/ 90 2.0)))
((= an2 64) (command "circle" pt "d" 92 "polygon" "6" pt "c" (/ 95 2.0)))
((= an2 68) (command "circle" pt "d" 97 "polygon" "6" pt "c" (/ 100 2.0)))
((= an2 72) (command "circle" pt "d" 102 "polygon" "6" pt "c" (/ 105 2.0)))
((= an2 76) (command "circle" pt "d" 107 "polygon" "6" pt "c" (/ 110 2.0)))
((= an2 80) (command "circle" pt "d" 112 "polygon" "6" pt "c" (/ 115 2.0)))
) ;cond
(princ (strcat "\nYou select: M" (rtos an2))) 
) ;progn
) ;if
;------------Hex socket head bolt --elevation
(if (= an1 "C")
(progn
(prompt "\nBolt Range: 3,4,5,6,8,10,12,14,16,18,20,22,24,27,30,33,36,39,42,45,48,52")   
(initget 7)
(setq an2 (getreal "\nBolt diameter (mm) : " )) 
(initget 7)
(setq bl (getreal "\nBolt length (mm) : " ))
(cond
((= an2 3)  (setq size '( 3 5.5 3 0.2 2.5 1.6 0.5 0.6)))
((= an2 4)  (setq size '( 4 7 4 0.3 3 2.2 0.7 0.8)))
((= an2 5)  (setq size '( 5 8.5 5 0.3 4 2.5 0.8 0.9)))
((= an2 6)  (setq size '( 6 10 6 0.4 5 3 1.0 1)))
((= an2 8)  (setq size '( 8 13 8 0.5 6 4 1.25 1.2)))
((= an2 10) (setq size '( 10 16 10 0.6 8 5 1.5 1.5)))
((= an2 12) (setq size '( 12 18 12 0.7 10 6 1.75 2)))
((= an2 14) (setq size '( 14 21 14 0.8 12 7 2.0 2)))
((= an2 16) (setq size '( 16 24 16 1 14 8 2.0 2)))
((= an2 18) (setq size '( 18 27 18 1 14 9 2.5 2.5)))
((= an2 20) (setq size '( 20 30 20 1 17 10 2.5 2.5)))
((= an2 22) (setq size '( 22 33 22 1 19 12 2.5 2.5)))
((= an2 24) (setq size '( 24 36 24 1 19 12 3.0 3)))
((= an2 27) (setq size '( 27 40 27 1.5 19 13.5 3.0 3)))
((= an2 30) (setq size '( 30 45 30 1.5 22 15 3.5 3.5)))
((= an2 33) (setq size '( 33 50 33 1.5 24 16.5 3.5 3.5)))
((= an2 36) (setq size '( 36 54 36 1.5 27 18 4.0 4)))
((= an2 39) (setq size '( 39 58 39 1.5 27 20 4.0 4)))
((= an2 42) (setq size '( 42 63 42 2 32 21 4.5 4.5)))
((= an2 45) (setq size '( 45 68 45 2 32 23 4.5 4.5)))
((= an2 48) (setq size '( 48 72 48 2 36 24 5.0 5)))
((= an2 52) (setq size '( 52 78 52 2.5 36 26 5.0 5)))
) ;cond
(setq d1 (nth 0 size)
       d (nth 1 size)
       h (nth 2 size)
       e (nth 3 size)
       b (nth 4 size) 
       m (nth 5 size)
      ph (nth 6 size)
       k (nth 7 size)
      gd (- d1 (* 1.082532 ph))                   ;;gol dia.
      dg (* (/ (/ b 2.0) (cos (dtr 30.0))) 2.0)   ;;hex diagonal dia. 
      hc (/ e (cos (dtr 60.0)))                   ;;head chamfer length
      dp (/ (- d1 gd) 2.0)                        ;;gol depth
     dk1 (/ (/ b 2.0) (cos (dtr 30.0)))           ;;socket width
      sl (/ (/ dg 2.0) (cos (dtr 30.0))))         ;;socket slop length
(graphscr)
(initget 1)
(setvar "osmode" oosmo)
(setq pt (getpoint "\nPick head lower center : " ))
(setvar "osmode" 0)
(setq p1 (polar pt (dtr 0.0) (/ d 2.0))
     p2a (polar p1 (dtr 90.0) h)
      p2 (polar p2a (dtr 270.0) e)
      p3 (polar p2 (dtr 150.0) hc)
      p5 (polar p2 (dtr 180.0) d)
      p4 (polar p5 (dtr 30.0) hc) 
      p6 (polar p1 (dtr 180.0) d)
      p7 (polar p6 (dtr 0.0) (/ (- d d1) 2.0))
      p8 (polar p7 (dtr 270.0) (- bl k))
     p13 (polar p7 (dtr 0.0) dp)
     p23 (polar p7 (dtr 270.0) bl)
      p9 (polar p23 (dtr 0.0) k)
     p10 (polar p9 (dtr 0.0) (- d1 (* k 2.0)))
     p11 (polar p8 (dtr 0.0) d1)
     P12 (polar p7 (dtr 0.0) d1)
     P14 (polar p13 (dtr 270.0) (- bl (- k dp))) 
     p15 (polar p13 (dtr 0.0) gd)
     p16 (polar p14 (dtr 0.0) gd)
     p24 (polar p6 (dtr 90.0) h)
     p17 (polar p24 (dtr 0.0) (+ (/ (- d dg) 2.0) dg))
     p18 (polar p17 (dtr 270.0) m)
     p19 (polar p24 (dtr 0.0) (+ (/ (- d dk1) 2.0) dk1))
     p20 (polar p19 (dtr 270.0) m)
     p21 (polar p18 (dtr 180.0) (/ dg 2.0))
     p22 (polar p18 (dtr 210.0) sl)
     p25 (polar p18 (dtr 180.0) dg))
(command "line" p3 p2 "")   (setq e19 (entlast))
(command "line" p11 p10 "") (setq e20 (entlast))
(command "line" p1 p2 "")   (setq e1 (entlast))
(command "line"	p3 p4 "")   (setq e2 (entlast))
(command "line" p5 p6 "")   (setq e3 (entlast))
(command "line" p6 p1 "")   (setq e4 (entlast))
(command "line" p7 p8 "")   (setq e5 (entlast))
(command "line"	p9 p10 "")  (setq e6 (entlast))
(command "line" p11 p12 "") (setq e7 (entlast))
(command "line" p5 p2 "")   (setq e8 (entlast))            
(command "line" p13 p14 "") (setq e9 (entlast))            
(command "line" p15 p16 "") (setq e10 (entlast))            
(command "line" p8 p11 "")  (setq e11 (entlast))           
(command "line" p17 p18 "") (setq e12 (entlast))    
(command "line" p19 p20 "") (setq e13 (entlast))
(command "line" p18 p21 "") (setq e14 (entlast))
(command "mirror" e12 "" p21 pt "") (setq e15 (entlast))
(command "mirror" e13 "" p21 pt "") (setq e16 (entlast))
(command "mirror" e14 "" p21 pt "") (setq e17 (entlast))
(command "mirror" e19 "" p21 pt "") (setq e21 (entlast))
(command "mirror" e20 "" p21 pt "") (setq e22 (entlast))
(command "arc" p18 p22 p25)         (setq e18 (entlast))
(command "rotate" e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 e11 e12 e13 e14 e15 e16 e17 e18 e19 e20 e21 e22 "" pt "\\")
(setq an (strcase (getstring "\nCalculate The Strength? <Y/N> <Y>: ")))
 (if (or (= an "Y")(= an ""))
  (progn
   (textpage) 
   (princ (strcat "\n▼최소 인장하중 (N/mm^2)==================="
		  "\n KS B 0233(탄소강)"
                  "\n-------------------------------------------"
		  "\n4T:392   6T:588   7T:686   10T:981"
		  "\n-------------------------------------------"		  
		  "\n ISO 898-1(탄소강)"
                  "\n-------------------------------------------"
                  "\n3.6:330  4.6:400  4.8:420  5.6:500  5.8:520"
		  "\n6.8:600  8.8(d<16):800     8.8(d>16):830"
		  "\n9.8:900  10.9:1040  12.9:1220"
		  "\n-------------------------------------------"		  
		  "\n ISO 898-1(스테인레스/오스테나이트)"
                  "\n-------------------------------------------"
                  "\nA1:500   A2,A3:700   A4,A5:800"
		  "\n==========================================="))
     (setq ts (getreal "\n최소 인장하중/위표값 (N/mm^2): ")
           sf (getreal "\n안전율 (최소인장하중에 대한): ")
    	   sa (/ (/ ts sf) 9.8)	;(kg/mm^2)
	   wf (* (/ pi 4.0)(expt (/ gd d1) 2.0) (expt d1 2.0) sa));:d1=외경/gd=골지름/sa=허용하중(kg/mm^2)
     (graphscr)
  )   
 )  
(princ (strcat "\nYou select: M" (rtos d1) "x" (rtos bl) "xP" (rtos ph)))
 (if (/= wf nil) (princ (strcat "  허용 인장하중(kgf)= " (rtos wf 2 2))))
) ;progn
) ;if
;------------ Hex.socket head bolt --top
(if (= an1 "D")  
(progn
(prompt "\nBolt Range: 3,4,5,6,8,10,12,14,16,18,20,22,24,27,30,33,36,39,42,45,48,52")     
(initget 7)
(setq an2 (getreal "\nBolt diameter (mm) : " )) 
(graphscr) 
(initget 1)
(setvar "osmode" oosmo)
(setq pt (getpoint "\nPick head center : " ))
(setvar "osmode" 0)
(cond
((= an2 3)  (command "circle" pt "d" 5.5 "polygon" "6" pt "c" (/ 2.5 2.0)))
((= an2 4)  (command "circle" pt "d" 7.0 "polygon" "6" pt "c" (/ 3.0 2.0)))
((= an2 5)  (command "circle" pt "d" 8.5 "polygon" "6" pt "c" (/ 4.0 2.0)))
((= an2 6)  (command "circle" pt "d" 10  "polygon" "6" pt "c" (/ 5.0 2.0)))
((= an2 8)  (command "circle" pt "d" 13  "polygon" "6" pt "c" (/ 6.0 2.0)))
((= an2 10) (command "circle" pt "d" 16  "polygon" "6" pt "c" (/ 8.0 2.0)))
((= an2 12) (command "circle" pt "d" 18  "polygon" "6" pt "c" (/ 10 2.0)))
((= an2 14) (command "circle" pt "d" 21  "polygon" "6" pt "c" (/ 12 2.0)))
((= an2 16) (command "circle" pt "d" 24  "polygon" "6" pt "c" (/ 14 2.0)))
((= an2 18) (command "circle" pt "d" 27  "polygon" "6" pt "c" (/ 14 2.0)))
((= an2 20) (command "circle" pt "d" 30  "polygon" "6" pt "c" (/ 17 2.0)))
((= an2 22) (command "circle" pt "d" 33  "polygon" "6" pt "c" (/ 17 2.0)))
((= an2 24) (command "circle" pt "d" 36  "polygon" "6" pt "c" (/ 19 2.0)))
((= an2 27) (command "circle" pt "d" 40  "polygon" "6" pt "c" (/ 19 2.0)))
((= an2 30) (command "circle" pt "d" 45  "polygon" "6" pt "c" (/ 22 2.0)))
((= an2 33) (command "circle" pt "d" 50  "polygon" "6" pt "c" (/ 24 2.0)))
((= an2 36) (command "circle" pt "d" 54  "polygon" "6" pt "c" (/ 27 2.0)))
((= an2 39) (command "circle" pt "d" 58  "polygon" "6" pt "c" (/ 27 2.0)))
((= an2 42) (command "circle" pt "d" 63  "polygon" "6" pt "c" (/ 32 2.0)))
((= an2 45) (command "circle" pt "d" 68  "polygon" "6" pt "c" (/ 32 2.0)))
((= an2 48) (command "circle" pt "d" 72  "polygon" "6" pt "c" (/ 36 2.0)))
((= an2 52) (command "circle" pt "d" 78  "polygon" "6" pt "c" (/ 36 2.0)))
) ;cond
(princ (strcat "\nYou select: M" (rtos an2))) 
) ;progn
) ;if
;------------ wrench bolt elev.
(if (= an1 "CC")  
(progn
(prompt "\nBolt Range: 3,4,5,6,8,10,12,14,16,18,20,22,24,27,30,33,36,39,42,45,48,52")
;(setq an 6)
(while 
(setq an2 (getint "\nBolt Diameter (mm): " ))
;(if (= an2 nil) (setq an2 (atoi an)))
(initget 7)
(setq tk (getreal "\nPlate Thickness (mm) : " ))
(cond
((= an2 3)  (setq size '( 3 5.5 3 0.2 2.5 1.6 0.5 0.6)))
((= an2 4)  (setq size '( 4 7 4 0.3 3 2.2 0.7 0.8)))
((= an2 5)  (setq size '( 5 8.5 5 0.3 4 2.5 0.8 0.9)))
((= an2 6)  (setq size '( 6 10 6 0.4 5 3 1.0 1)))
((= an2 8)  (setq size '( 8 13 8 0.5 6 4 1.25 1.2)))
((= an2 10) (setq size '( 10 16 10 0.6 8 5 1.5 1.5)))
((= an2 12) (setq size '( 12 18 12 0.7 10 6 1.75 2)))
((= an2 14) (setq size '( 14 21 14 0.8 12 7 2.0 2)))
((= an2 16) (setq size '( 16 24 16 1 14 8 2.0 2)))
((= an2 18) (setq size '( 18 27 18 1 14 9 2.5 2.5)))
((= an2 20) (setq size '( 20 30 20 1 17 10 2.5 2.5)))
((= an2 22) (setq size '( 22 33 22 1 19 12 2.5 2.5)))
((= an2 24) (setq size '( 24 36 24 1 19 12 3.0 3)))
((= an2 27) (setq size '( 27 40 27 1.5 19 13.5 3.0 3)))
((= an2 30) (setq size '( 30 45 30 1.5 22 15 3.5 3.5)))
((= an2 33) (setq size '( 33 50 33 1.5 24 16.5 3.5 3.5)))
((= an2 36) (setq size '( 36 54 36 1.5 27 18 4.0 4)))
((= an2 39) (setq size '( 39 58 39 1.5 27 20 4.0 4)))
((= an2 42) (setq size '( 42 63 42 2 32 21 4.5 4.5)))
((= an2 45) (setq size '( 45 68 45 2 32 23 4.5 4.5)))
((= an2 48) (setq size '( 48 72 48 2 36 24 5.0 5)))
((= an2 52) (setq size '( 52 78 52 2.5 36 26 5.0 5)))
) ;cond
(setq d1 (nth 0 size) ;bolt n.d
       d (nth 1 size) ;head dia.
       h (nth 2 size) ;head height
      hd (+ d 1.0)    ;head hole
      bh (+ d1 1.0)   ;bolt hole
      hh (+ h 1.0))   ;head depth
(graphscr)
(setvar "osmode" oosmo)
(setq pt (getpoint "\nPick 'Plate Top Center' : " ))
(setvar "osmode" 0)
(setq p1 (polar pt (dtr 180.0) (/ hd 2.0))     
      p2 (polar P1 (dtr 0) hd)
      p3 (polar p2 (dtr 270.0) hh)
      p4 (polar p3 (dtr 180.0) hd)
      p5 (polar pt (dtr 270.0) hh)
      p6 (polar p5 (dtr 180.0) (/ bh 2.0))
      p7 (polar p6 (dtr 0) bh)
      p8 (polar p7 (dtr 270.0) (- tk hh))
      p9 (polar p8 (dtr 180.0) bh))
(command "pline" p2 p3 P3 p4 p4 p1 "")  (setq e1 (entlast))
(command "line" p7 p8 "") (setq e2 (entlast))
(command "line" p6 p9 "") (setq e3 (entlast))
(command "rotate" e1 e2 e3 "" pt "\\")
;(setq an an2)
(princ (strcat "\nYou select: M" (rtos an2)))
)
) ;progn
) ;if
;-------------Hex.nut (1st)--elevation
(if (= an1 "E")  
(progn
(prompt "\nNut Range: 2,2.2,2.3,2.5,2.6,3,3.5,4,4.5,5,6,7,8,10,12,14,16,18,20,22,24,27,30,33,36,39,42,45,48,52,56,60,64,68,72,76,80,85,90,95,100,105,110,115,120,125,130")  
(initget 7)
(setq an2 (getreal "\nBolt diameter (mm) : " )) 
(cond
((= an2 2)  (setq size '( 2 1.6 4 3.8))) 
((= an2 2.2)(setq size '( 2.2 1.8 4.5 4.3))) 
((= an2 2.3)(setq size '( 2.3 1.8 4.5 4.3))) 
((= an2 2.5)(setq size '( 2.5 2 5 4.7))) 
((= an2 2.6)(setq size '( 2.6 2 5 4.7))) 
((= an2 3)  (setq size '( 3 2.4 5.5 5.3)))
((= an2 3.5)(setq size '( 3 2.8 6 5.8)))
((= an2 4)  (setq size '( 4 3.2 7 6.8)))
((= an2 4.5)(setq size '( 4.5 3.6 8 7.8)))
((= an2 5)  (setq size '( 5 4 8 7.8)))
((= an2 6)  (setq size '( 6 5 10 9.8)))
((= an2 7)  (setq size '( 7 5.5 11 10.8)))
((= an2 8)  (setq size '( 8 6.5 13 12.5)))
((= an2 10) (setq size '( 10 8 17 16.5)))
((= an2 12) (setq size '( 12 10 19 18)))
((= an2 14) (setq size '( 14 11 22 21)))
((= an2 16) (setq size '( 16 13 24 23)))
((= an2 18) (setq size '( 18 15 27 26)))
((= an2 20) (setq size '( 20 16 30 29)))
((= an2 22) (setq size '( 22 18 32 31)))
((= an2 24) (setq size '( 24 19 36 34)))
((= an2 27) (setq size '( 27 22 41 39)))
((= an2 30) (setq size '( 30 24 46 44)))
((= an2 33) (setq size '( 33 26 50 48)))
((= an2 36) (setq size '( 36 29 55 53)))
((= an2 39) (setq size '( 39 31 60 57)))
((= an2 42) (setq size '( 42 32 65 62)))
((= an2 45) (setq size '( 45 36 70 67)))
((= an2 48) (setq size '( 48 38 75 72)))
((= an2 52) (setq size '( 52 42 80 77)))
((= an2 56) (setq size '( 56 45 85 82)))
((= an2 60) (setq size '( 60 48 90 87)))
((= an2 64) (setq size '( 64 51 95 92)))
((= an2 68) (setq size '( 68 54 100 97))) 
((= an2 72) (setq size '( 72 58 105 102)))
((= an2 76) (setq size '( 76 61 110 107)))
((= an2 80) (setq size '( 80 64 115 112))) 
((= an2 85) (setq size '( 85 68 120 116)))
((= an2 90) (setq size '( 90 72 130 126)))
((= an2 95) (setq size '( 95 76 135 131)))
((= an2 100) (setq size '( 100 80 145 141)))
((= an2 105) (setq size '( 105 84 150 146)))
((= an2 110) (setq size '( 110 88 155 151)))
((= an2 115) (setq size '( 115 92 165 161))) 
((= an2 120) (setq size '( 120 96 170 166)))
((= an2 125) (setq size '( 125 100 180 176)))
((= an2 130) (setq size '( 130 104 185 181))) 
) ;cond
(setq d1 (nth 0 size)
       h (nth 1 size)
       b (nth 2 size)
       d (nth 3 size) 
      dg (* (/ (/ b 2.0) (cos (dtr 30.0))) 2.0)   ;;diagonal dia.
      hc (/ (/ (- dg d) 2.0) (sin (dtr 60.0)))    ;;head chamber length
      h1 (* hc (sin (dtr 30.0))))                 ;;r height
(graphscr)
(initget 1)
(setvar "osmode" oosmo)
(setq pt (getpoint "\nPick nut lower center : " ))
(setvar "osmode" 0)
(setq p1 (polar pt (dtr 0.0) (/ d1 2.0))
      p2 (polar pt (dtr 0.0) (/ dg 2.0))
     p3a (polar p2 (dtr 90.0) h)
      p4 (polar p3a (dtr 180.0) (/ (- dg d) 2.0))
     p3b (polar p4 (dtr -30.0) hc) 
      p5 (polar p1 (dtr 90.0) (- h h1))
      p6 (polar p5 (dtr 180.0) d1)
      p7 (polar p4 (dtr 180.0) d)
     p8b (polar p3b (dtr 180.0) dg)  
      p9 (polar p2 (dtr 180.0) dg)
     p10 (polar p1 (dtr 180.0) d1)
     p16 (polar p1 (dtr 180.0) 0.5)
     P19 (polar pt (dtr 90.0) h)
     P22 (polar p4 (dtr 180.0) 1.0)
     p23 (polar p4 (dtr -30.0) (/ hc 2.0)))
(command "line" p2 p3b "") (setq e1 (entlast))  
(command "line" p3b p4 "") (setq e2 (entlast)) 
(command "line" p7 p4 "")  (setq e3 (entlast))  
(command "line" p6 p10 "") (setq e4 (entlast)) 
(command "line" p5 p1 "")  (setq e5 (entlast)) 
(command "circle" "3p" p5 "_tan" p22 "_tan" p23)  (setq e6 (entlast))
(setq cent (entget e6))
(setq cpt (cdr (assoc 10 cent))) ;cen point
(setq crd (cdr (assoc 40 cent))) ;radius
(setq pt1 (polar cpt (dtr 60.0) crd))
(command "erase" e6 "")
(command "arc" pt1 "ce" cpt p5)    (setq e7 (entlast))
(command "arc" p5 p19 p6)          (setq e8 (entlast))
(command "mirror" e7 "" p19 pt "") (setq e9 (entlast))
(command "mirror" e1 "" p19 pt "") (setq e11 (entlast))
(command "line" p7 p8b "")         (setq e12 (entlast)) 
(command "line" p9 p2 "")          (setq e10 (entlast))
(command "rotate" e1 e2 e3 e4 e5 e7 e8 e9 e10 e11 e12 "" pt "\\")
;weight--
(setq wt1 (/ (* 0.8660254038 (expt dg 2.0) h 7.85) 1000000.0));head w.t
(setq a (/ (* pi (expt d1 2.0)) 4.0))
(setq wt2 (/ (* a h 7.85) 1000000.0));tap hole
(setq twt (- wt1 wt2))
(princ (strcat "\nYou select: M" (rtos d1) " (≒" (rtos twt) "kg / @S=7.85)"))
) ;progn
) ;if
;------------ Hex.nut (2nd)--elevation 
(if (= an1 "F")  
(progn
(prompt "\nNut Range: 2,2.2,2.3,2.5,2.6,3,3.5,4,4.5,5,6,7,8,10,12,14,16,18,20
                      ,22,24,27,30,33,36,39,42,45,48,52,56,60,64,68,72,76,80,85
                      ,90,95,100,105,110,115,120,125,130")   
(initget 7)
(setq an2 (getreal "\nBolt diameter (mm) : " )) 
(cond
((= an2 2)  (setq size '( 2 1.6 4 3.8))) 
((= an2 2.2)(setq size '( 2.2 1.8 4.5 4.3))) 
((= an2 2.3)(setq size '( 2.3 1.8 4.5 4.3))) 
((= an2 2.5)(setq size '( 2.5 2 5 4.7))) 
((= an2 2.6)(setq size '( 2.6 2 5 4.7))) 
((= an2 3)  (setq size '( 3 2.4 5.5 5.3)))
((= an2 3.5)(setq size '( 3 2.8 6 5.8)))
((= an2 4)  (setq size '( 4 3.2 7 6.8)))
((= an2 4.5)(setq size '( 4.5 3.6 8 7.8)))
((= an2 5)  (setq size '( 5 4 8 7.8)))
((= an2 6)  (setq size '( 6 5 10 9.8)))
((= an2 7)  (setq size '( 7 5.5 11 10.8)))
((= an2 8)  (setq size '( 8 6.5 13 12.5)))
((= an2 10) (setq size '( 10 8 17 16.5)))
((= an2 12) (setq size '( 12 10 19 18)))
((= an2 14) (setq size '( 14 11 22 21)))
((= an2 16) (setq size '( 16 13 24 23)))
((= an2 18) (setq size '( 18 15 27 26)))
((= an2 20) (setq size '( 20 16 30 29)))
((= an2 22) (setq size '( 22 18 32 31)))
((= an2 24) (setq size '( 24 19 36 34)))
((= an2 27) (setq size '( 27 22 41 39)))
((= an2 30) (setq size '( 30 24 46 44)))
((= an2 33) (setq size '( 33 26 50 48)))
((= an2 36) (setq size '( 36 29 55 53)))
((= an2 39) (setq size '( 39 31 60 57)))
((= an2 42) (setq size '( 42 32 65 62)))
((= an2 45) (setq size '( 45 36 70 67)))
((= an2 48) (setq size '( 48 38 75 72)))
((= an2 52) (setq size '( 52 42 80 77)))
((= an2 56) (setq size '( 56 45 85 82)))
((= an2 60) (setq size '( 60 48 90 87)))
((= an2 64) (setq size '( 64 51 95 92)))
((= an2 68) (setq size '( 68 54 100 97))) 
((= an2 72) (setq size '( 72 58 105 102)))
((= an2 76) (setq size '( 76 61 110 107)))
((= an2 80) (setq size '( 80 64 115 112))) 
((= an2 85) (setq size '( 85 68 120 116)))
((= an2 90) (setq size '( 90 72 130 126)))
((= an2 95) (setq size '( 95 76 135 131)))
((= an2 100) (setq size '( 100 80 145 141)))
((= an2 105) (setq size '( 105 84 150 146)))
((= an2 110) (setq size '( 110 88 155 151)))
((= an2 115) (setq size '( 115 92 165 161))) 
((= an2 120) (setq size '( 120 96 170 166)))
((= an2 125) (setq size '( 125 100 180 176)))
((= an2 130) (setq size '( 130 104 185 181))) 
) ;cond
(setq d1 (nth 0 size)
       h (nth 1 size)
       b (nth 2 size)
       d (nth 3 size) 
      dg (* (/ (/ b 2.0) (cos (dtr 30.0))) 2.0) ;;diagonal dia.
      hc (/ (/ (- dg d) 2.0) (sin (dtr 60.0)))  ;;head chamber length
      h1 (* hc (sin (dtr 30.0))))               ;;r height
(graphscr)
(initget 1)
(setvar "osmode" oosmo)
(setq pt (getpoint "\nPick nut lower center : " ))
(setvar "osmode" 0)
(setq p1a (polar pt (dtr 0.0) (/ d1 2.0))
       p1 (polar p1a (dtr 90.0) h1)
      p2a (polar pt (dtr 0.0) (/ dg 2.0))
       p2 (polar p2a (dtr 90.0) h1)
      p3a (polar p2a (dtr 90.0) h)
       p4 (polar p3a (dtr 180.0) (/ (- dg d) 2.0))
      P4u (polar p4 (dtr 270.0) h) 
      p3b (polar p4 (dtr -30.0) hc) 
       p5 (polar p1a (dtr 90.0) (- h h1))
       p6 (polar p5 (dtr 180.0) d1)
       p7 (polar p4 (dtr 180.0) d)
      p7u (polar p7 (dtr 270.0) h)
      p8a (polar p3a (dtr 180.0) dg)
      p8b (polar p3b (dtr 180.0) dg)  
       p9 (polar p2 (dtr 180.0) dg)
      p10 (polar p1 (dtr 180.0) d1)
      p16 (polar p1a (dtr 180.0) 0.5)
      P19 (polar pt (dtr 90.0) h)
      P22 (polar p4 (dtr 180.0) 1.0)
      p23 (polar p4 (dtr -30.0) (/ hc 2.0))
       m1 (polar p8a (dtr 270.0) (/ h 2.0))
       m2 (polar m1 (dtr 0.0) dg))
(command "line" p2 p3b "") (setq e1 (entlast))  
(command "line" p3b p4 "") (setq e2 (entlast))
(command "line" p4 p7 "")  (setq e3 (entlast)) 
(command "line" p7 p8b "") (setq e4 (entlast))
(command "line" p8b p9 "") (setq e5 (entlast)) 
(command "line" p5 p1 "")  (setq e6 (entlast)) 
(command "line" p6 p10 "") (setq e7 (entlast)) 
(command "circle" "3p" p5 "_tan" p22 "_tan" p23) (setq e8 (entlast))
(setq cent (entget e8))
(setq cpt (cdr (assoc 10 cent))) ;cen point
(setq crd (cdr (assoc 40 cent))) ;radius
(setq pt1 (polar cpt (dtr 60.0) crd))
(command "erase" e8 "")
(command "arc" p5 p19 p6)          (setq e10 (entlast))
(command "arc" pt1 "ce" cpt p5)    (setq e9 (entlast))
(command "mirror" e9 "" p19 pt "") (setq e11 (entlast))
(command "mirror" e10 "" m1 m2 "") (setq e12 (entlast))
(command "mirror" e9 "" m1 m2 "")  (setq e13 (entlast))
(command "mirror" e2 "" m1 m2 "")  (setq e14 (entlast))
(command "mirror" e11 "" m1 m2 "") (setq e15 (entlast))
(command "mirror" e4 "" m1 m2 "")  (setq e16 (entlast))
(command "mirror" e3 "" m1 m2 "")  (setq e17 (entlast)) 
(command "rotate" e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 e11 e12 e13 e14 e15 e16 e17 "" pt "\\")
;weight--
(setq wt1 (/ (* 0.8660254038 (expt dg 2.0) h 7.85) 1000000.0));head w.t
(setq a (/ (* pi (expt d1 2.0)) 4.0))
(setq wt2 (/ (* a h 7.85) 1000000.0));tap hole
(setq twt (- wt1 wt2))
(princ (strcat "\nYou select: M" (rtos d1) " (≒" (rtos twt) "kg / @S=7.85)"))
) ;progn
) ;if
;------------ Hex.nut --top 
(if (= an1 "G")  
(progn
(prompt "\nNut Range: 2,2.2,2.3,2.5,2.6,3,3.5,4,4.5,5,6,7,8,10,12,14,16,18,20,22,24,27,30,33,36,39,42,45,48,52,56,60,64,68,72,76,80,85,90,95,100,105,110,115,120,125,130")   
(initget 7)
(setq an2 (getreal "\nBolt diameter (mm) : " )) 
(tplist)
(setq pit (nth 3 size))
(setq pit (rtos pit))
(setq ph (getstring (strcat "\nPitch (mm) <" pit ">:" )))
 (if (= ph "")
  (setq ph (atof pit))
  (setq ph (atof ph)) 
 )  
(setq d1 (nth 0 size)
       b (nth 1 size)
       d (nth 2 size)
      dg (* (/ (/ b 2.0) (cos (dtr 30.0))) 2.0)   ;;diagonal dia.
      gd (- d1 (* 1.082532 ph)))                  ;;gol dia.
(graphscr)
(initget 1)
(setvar "osmode" oosmo)
(setq pt (getpoint "\nPick nut center : " ))
(setvar "osmode" 0)
(setq p1 (polar pt (dtr 0.0) (/ d1 2.0)))
(command "circle" pt "d" gd  
         "arc" p1 "c" pt "a" "270"
         "circle" pt "d" d
         "polygon" "6" pt "c" (/ b 2.0))
(princ (strcat "\nYou select: M" (rtos d1) "xP" (rtos ph)))
) ;progn
) ;if
;------------Tap depth --elevation 
(if (= an1 "H")  
(progn
(initget 7)
(setq an2 (getreal "\nBolt diameter (mm) : " ))
(tplist)
(setq d1 an2)
(setq pit (nth 3 size))
(setq pit (rtos pit))
(setq ph (getstring (strcat "\nPitch (mm) <" pit ">:" )))
  (if (= ph "")
    (setq ph (atof pit))
    (setq ph (atof ph)) 
  )
(initget 7)
(setq tad (getreal "\nTap depth (mm) : " ))
(initget 7)
(setq drd (getreal "\nDrill depth (mm) : " ))
(setq gd (- d1 (* 1.082532 ph))            ;;gol dia. 
      l1 (/ (- d1 gd) 2.0)
      l2 (/ l1 (cos (dtr 30.0)))           ;;tap diagonal length
      l3 (/ (/ gd 2.0) (cos (dtr 30.0))))  ;;dr diagonal length
(graphscr)
(initget 1)
(setvar "osmode" oosmo)
(setq pt (getpoint "\nPick upper center : " ))
(setvar "osmode" 0)
(setq p1 (polar pt (dtr 0.0) (/ gd 2.0))
      p2 (polar p1 (dtr 270.0) drd)
      p3 (polar p2 (dtr 180.0) gd) 
      p4 (polar p1 (dtr 180.0) gd) 
      p5 (polar pt (dtr 0.0) (/ d1 2.0)) 
      p6 (polar p5 (dtr 270.0) tad) 
      p7 (polar p6 (dtr 210.0) l2) 
      p8 (polar p5 (dtr 180.0) d1) 
      p9 (polar p8 (dtr 270.0) tad) 
     p10 (polar p7 (dtr 180.0) gd) 
     p11 (polar p2 (dtr 210.0) l3))
(command "line" p8 p9 "")  (setq e1 (entlast))  
(command "line" p6 p5 "")  (setq e2 (entlast))     
(command "line" p4 p3 "")  (setq e3 (entlast))  
(command "line" p11 p2 "") (setq e4 (entlast))  
(command "line" p2 p1 "")  (setq e5 (entlast))  
(command "line" p9 p6 "")  (setq e6 (entlast))          
(command "line" p9 p10 "") (setq e7 (entlast))           
(command "line" p6 p7 "")  (setq e8 (entlast))          
(command "line" p3 p2 "")  (setq e9 (entlast))
(command "line" p3 p11 "") (setq e10 (entlast))  
(command "rotate" e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 "" pt "\\")
(princ (strcat "\nYou select: M" (rtos d1) "xP" (rtos ph)))
) ;progn
) ;if
;------------Tap thru --elevation 
(if (= an1 "I")  
(progn
(initget 7)
(setq an2 (getreal "\nBolt diameter (mm) : " )) 
(tplist)
(setq d1 an2)
(setq pit (nth 3 size))
(setq pit (rtos pit))
(setq ph (getstring (strcat "\nPitch (mm) <" pit ">:" )))
  (if (= ph "")
    (setq ph (atof pit))
    (setq ph (atof ph)) 
  )
(initget 7)
(setq tad (getreal "\nTap depth (mm) : " ))
(setq gd (- d1 (* 1.082532 ph)))            ;;gol dia. 
(graphscr)
(initget 1)
(setvar "osmode" oosmo)
(setq pt (getpoint "\nPick upper center : " ))
(setvar "osmode" 0)
(setq p1 (polar pt (dtr 0.0) (/ gd 2.0))
      p2 (polar p1 (dtr 270.0) tad)
      p3 (polar p2 (dtr 180.0) gd) 
      p4 (polar p1 (dtr 180.0) gd) 
      p5 (polar pt (dtr 0.0) (/ d1 2.0)) 
      p6 (polar p5 (dtr 270.0) tad) 
      p7 (polar p5 (dtr 180.0) d1) 
      p8 (polar p7 (dtr 270.0) tad))
(command "line" p7 p8 "") (setq e1 (entlast))  
(command "line" p4 p3 "") (setq e2 (entlast))   
(command "line" p1 p2 "") (setq e3 (entlast))   
(command "line" p5 p6 "") (setq e4 (entlast))
(command "rotate" e1 e2 e3 e4 "" pt "\\")
(princ (strcat "\nYou select: M" (rtos d1) "xP" (rtos ph)))
) ;progn
) ;if
;------------Tap --top 
(if (= an1 "J")  
(progn
(initget 7)
(setq an2 (getreal "\nBolt diameter (mm) : " )) 
(tplist)
(setq d1 an2)
(setq pit (nth 3 size))
(setq pit (rtos pit))
(setq ph (getstring (strcat "\nPitch (mm) <" pit ">:" )))
  (if (= ph "")
    (setq ph (atof pit))
    (setq ph (atof ph)) 
  )
(setq gd (- d1 (* 1.082532 ph)))            ;;gol dia. 
(graphscr)
(initget 1)
(setvar "osmode" oosmo)
(setq pt (getpoint "\nPick tap center : " ))
(setvar "osmode" 0)
(setq p1 (polar pt (dtr 0.0) (/ d1 2.0))) 
(command "circle" pt "d" gd
         "arc" p1 "c" pt "a" "270")
(princ (strcat "\nYou select: M" (rtos d1) "xP" (rtos ph)))
) ;progn
) ;if
;------------Stop bolt --elevation
(if (= an1 "K")
(progn
(prompt "\nBolt Range: 3,4,5,6,8,10,12,14,16,18,20")    
(initget 7)
(setq an2 (getreal "\nBolt diameter (mm) : " )) 
(initget 7)
(setq bl (getreal "\nBolt length (mm) : " ))
(cond
((= an2 3)  (setq size '( 3 0.5 1.5 0.3 1.5 0.6)))
((= an2 4)  (setq size '( 4 0.7 2 0.3 2 0.8)))
((= an2 5)  (setq size '( 5 0.8 2.5 0.5 2.5 0.9)))
((= an2 6)  (setq size '( 6 1.0 3 0.5 3 1)))
((= an2 8)  (setq size '( 8 1.25 4 0.6 4 1.2)))
((= an2 10) (setq size '( 10 1.5 5 0.8 4 1.5)))
((= an2 12) (setq size '( 12 1.75 6 1 5 2)))
((= an2 14) (setq size '( 14 2.0 6 1 6 2)))
((= an2 16) (setq size '( 16 2.0 8 1.1 6 2)))
((= an2 18) (setq size '( 18 2.5 8 1.1 8 2.5)))
((= an2 20) (setq size '( 20 2.5 10 1.2 8 2.5)))
) ;cond
(setq d1 (nth 0 size)
      ph (nth 1 size)
       b (nth 2 size)
       e (nth 3 size)
       m (nth 4 size) 
       k (nth 5 size)
      gd (- d1 (* 1.082532 ph))                   ;;gol dia.
      dg (* (/ (/ b 2.0) (cos (dtr 30.0))) 2.0)   ;;hex diagonal dia. 
      dp (/ (- d1 gd) 2.0)                        ;;gol depth
     dk1 (/ (/ b 2.0) (cos (dtr 30.0)))           ;;socket width
      sl (/ (/ dg 2.0) (cos (dtr 30.0)))          ;;socket slop length
      lh (/ (- d1 gd) 2.0) 
      l1 (/ e (sin (dtr 30.0)))
      l2 (/ lh (cos (dtr 30.0)))  
      l3 (/ lh (cos (dtr 45.0))))  
(graphscr)
(initget 1)
(setvar "osmode" oosmo)
(setq pt (getpoint "\nPick upper center : " ))
(setvar "osmode" 0)
(setq p1 (polar pt (dtr 0.0) (/ d1 2.0))
      p2 (polar p1 (dtr 270.0) e)
      p3 (polar p2 (dtr 150.0) l1)
      p4 (polar p2 (dtr 150.0) l2) 
      p7 (polar p2 (dtr 180.0) d1) 
      p5 (polar p7 (dtr 30.0) l1)
      p6 (polar p7 (dtr 30.0) l2)
     p11 (polar p1 (dtr 270.0) bl) 
      p8 (polar p1 (dtr 270.0) (- bl k))
      p9 (polar p11 (dtr 180.0) k)
     p10 (polar p8 (dtr 225.0) l3)
     P12 (polar p11 (dtr 180.0) d1)
     P13 (polar p8 (dtr 180.0) d1) 
     p14 (polar p12 (dtr 0.0) k)
     p15 (polar p10 (dtr 180.0) gd)
     p16 (polar pt (dtr 0.0) (/ dg 2.0))
     p17 (polar p16 (dtr 270.0) m)
     p18 (polar pt (dtr 0.0) (/ dk1 2.0))
     p19 (polar p18 (dtr 270.0) m)
     p20 (polar pt (dtr 270.0) m)
     p21 (polar p17 (dtr 210.0) sl)
     p22 (polar p17 (dtr 180.0) dg))
(command "line" p2 p3 "")   (setq e1 (entlast))
(command "line" p3 p5 "")   (setq e19 (entlast))
(command "line" p9 p14 "")  (setq e20 (entlast)) 
(command "line" p5 p7 "")   (setq e2 (entlast))  
(command "line" p13 p14 "") (setq e3 (entlast))  
(command "line" p9 p8 "")   (setq e4 (entlast))   
(command "line" p8 p2 "")   (setq e5 (entlast))  	 
(command "line" p7 p2 "")   (setq e6 (entlast))  
(command "line" p13 p8 "")  (setq e7 (entlast))  
(command "line" p7 p13 "")  (setq e8 (entlast))   
(command "line" p6 p15 "")  (setq e9 (entlast))  
(command "line" p4 p10 "")  (setq e10 (entlast))  
(command "line" p2 p8 "")   (setq e11 (entlast))  
(command "line" p16 p17 "") (setq e12 (entlast))    
(command "line" p18 p19 "") (setq e13 (entlast))
(command "line" p20 p17 "") (setq e14 (entlast))
(command "mirror" e12 "" pt p20 "") (setq e15 (entlast))
(command "mirror" e13 "" pt p20 "") (setq e16 (entlast))
(command "mirror" e14 "" pt p20 "") (setq e17 (entlast))
(command "arc" p17 p21 p22)         (setq e18 (entlast))
(command "rotate" e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 e11 e12 e13 e14 e15 e16 e17 e18 e19 e20 "" pt "\\")
(princ (strcat "\nYou select: M" (rtos d1) "x" (rtos bl) "xP" (rtos ph)))
) ;progn
) ;if
;-----------------------------------------------------------------------------------------------
(if (= an1 "L")   
(progn
 (princ "\n경첩볼트")  
(setq ans (getint "\n Select No. <1:M8  2:M10  3:M12  4:M16  5:M20  6:M22  7:M24>: " ))
(cond
((= ans 1)  (setq size '( 8 1.25 16 8 8 100 )))
((= ans 2)  (setq size '( 10 1.5 20 10 10 100)))
((= ans 3)  (setq size '( 12 1.75 26 12 12 150)))
((= ans 4)  (setq size '( 16 2 32 16 16 150)))
((= ans 5)  (setq size '( 20 2.5 40 20 20 150)))
((= ans 6)  (setq size '( 22 2.5 42 22 22 200)))
((= ans 7)  (setq size '( 24 3 50 24 24 200)))
) ;cond
(setq  m  (nth 0 size) ;호칭
       ph (nth 1 size) ;피치
       d2 (nth 2 size) ;바깥경
       d1 (nth 3 size) ;내경
       w  (nth 4 size) ;두께
       l  (nth 5 size) ;length-cen to end 
       gd (- m (* 1.082532 ph)) ;gol dia.
       c  (/ (- m gd) 2.0)      ;chamber
       a1  (expt (/ d2 2.0) 2.0)
       a2  (expt (/ m 2.0) 2.0)
       a   (sqrt (- a1 a2)) ;볼트와 링의 중심에서 접촉점 길이
)
(graphscr) 
(initget 1)
(setvar "osmode" oosmo) 
(setq pt (getpoint "\nPick anywhere: " ))
(setvar "osmode" 0) 
(setq p1 (polar pt (dtr 0.0) a)    
      p2 (polar p1 (dtr 90.0) (/ m 2.0))
      p3 (polar p2 (dtr -90.0) m)      
      p4 (polar p3 (dtr 0.0) (- l a))      
      p5 (polar p4 (dtr 90.0) m)      
      p6 (polar p5 (dtr 180.0) (- (- l a) 10))      
      p7 (polar p6 (dtr -90.0) m)      
      p8 (polar pt (dtr 0.0) (/ d2 2.0));trim point
      p9 (polar p5 (dtr -90.0) (/ (- m gd) 2.0));골내경
     p11 (polar p9 (dtr -90.0) gd);골내경
     p10 (polar p9 (dtr 180.0) (- (- l a) 10));골내경
     p12 (polar p10 (dtr -90.0) gd);골내경
     p13 (polar p5 (dtr 180.0) c)
     p14 (polar p13 (dtr -90.0) m)
     p15 (polar p5 (dtr -90.0) (/ m 2.0))
     p15-1 (polar p15 (dtr 0.0) 5.0) ;center 
     p16 (polar pt (dtr 180.0) (/ d2 2.0))
     p16-1 (polar p16 (dtr 180.0) 5.0) ;center
     p17 (polar pt (dtr 90.0) (+ (/ d2 2.0) 5.0)) ;center
     p18 (polar pt (dtr -90.0) (+ (/ d2 2.0) 5.0));center
 )
(command "circle" pt "d" d1)  (setq e1 (entlast))
(command "circle" pt "d" d2)  (setq e2 (entlast))  
(command "line" p2 p3 "")     (setq e3 (entlast))  
(command "trim" e3 "" "f" pt p15 "" "")
(command "line"	p2 p5 "")     (setq e5 (entlast))  
(command "line" p3 p4 "")     (setq e6 (entlast))  
(command "line" p5 p4 "")     (setq e7 (entlast))  
(command "line" p9 p10 "")    (setq e8 (entlast))  
(command "line"	p11 p12 "")   (setq e9 (entlast))  
(command "line" p6 p7 "")     (setq e10 (entlast))  
(command "line" p13 p14 "")   (setq e11 (entlast))
(command "chamfer" "d" c c) 
(command "chamfer" e5 e7)     (setq e12 (entlast))
(command "chamfer" e7 e6)     (setq e13 (entlast))
(command "copy" e12 "" p9 p10)(setq e14 (entlast))
(command "copy" e13 "" p11 p12) (setq e15 (entlast))
(command "line" p15-1 p16-1 "") (setq e16 (entlast));center
(command "chprop" e16 "" "la" "cen-m" "") ;center
(command "line" p17 p18 "")     (setq e17 (entlast));center
(command "chprop" e17 "" "la" "cen-m" "") ;center  
(command "rotate" e1 e2 e3 e5 e6 e7 e8 e9 e10 e11 e12 e13 e14 e15 e16 e17 "" pt "\\")
(princ (strcat "\nYou select: M" (rtos m) "x" (rtos l) " (HEAD DEPTH:" (rtos m) "mm) -길면 짤라씁시다")) 
)
);end if(hinge bolt)  
;------------------------------------------------------------------------------
 (if (= an1 "O") ;ibolt
  (progn
    ;;;(initget 7)
 (setq ans (getint (strcat "\nBolt Dia.(Metric)  <8,10,12,16,20,24,30,36,"
                                                 "42,48,64,80,(90),100>: " )))
 (cond
 ((= ans 8)  (setq size '( "M8" 32.6 20 6.3 16 5 17 33.3 15 3 6 1 4 1.2 1.25 80)))
 ((= ans 10) (setq size '( "M10" 41 25 8 20 7 21 41.5 18 4 7.7 1.2 4 1.5 1.5 150)))
 ((= ans 12) (setq size '( "M12" 50 30 10 25 9 26 51 22 5 9.4 1.4 6 2 1.75 220)))
 ((= ans 16) (setq size '( "M16" 60 35 12.5 30 11 30 60 27 5 13 1.6 6 2 2 450)))
 ((= ans 20) (setq size '( "M20" 72 40 16 35 13 35 71 30 6 16.4 2 8 2.5 2.5 630)))
 ((= ans 24) (setq size '( "M24" 90 50 20 45 18 45 90 38 8 19.6 2.5 12 3 3 950)))
 ((= ans 30) (setq size '( "M30" 110 60 25 60 22 55 110 45 8 25 3 15 3.5 3.5 1500)))
 ((= ans 36) (setq size '( "M36" 133 70 31.5 70 26 65 131.5 55 10 30.3 3 18 4 4 2300)))
 ((= ans 42) (setq size '( "M42" 151 80 35.5 80 30 75 150.5 65 12 35.6 3.5 20 4.5 4.5 3400)))
 ((= ans 48) (setq size '( "M48" 170 90 40 90 35 85 170 70 12 41 4 22 5 5 4500)))
 ((= ans 64) (setq size '( "M64" 210 110 50 110 42 105 210 90 14 55.7 5 25 6 6 9000)))
 ((= ans 80) (setq size '( "M80" 266 140 63 130 50 130 263 105 14 71 5 35 6 6 15000)))
 ((= ans 90) (setq size '( "M90" 302 160 71 150 55 150 301 120 14 81 5 35 6 6 18000)))
 ((= ans 100)(setq size '( "M100" 340 180 80 170 60 165 335 130 14 91 5 40 6 6 20000)))
 ) ;cond
 (setq nd (nth 0 size)  
        a (nth 1 size)
        b (nth 2 size)
        c (nth 3 size)
        d (nth 4 size)
        thk (nth 5 size)
        h (nth 6 size)
       ;H (nth 7 size)   ;no using
        l (nth 8 size)
        e (nth 9 size)
        g (nth 10 size)
       r1 (nth 11 size)
      ;r2 (nth 12 size)  ;no using
        k (nth 13 size)
       ph (nth 14 size)            ; pitch
       ld (nth 15 size)            ; load(kg)
      ;a1 (+ (/ a 2.0) 5.0)        ; no using
       gd (- ans (* 1.082532 ph))  ; gol dia.
       of (/ (- ans gd) 2.0))
    (graphscr)
   (initget 1)
   (setvar "osmode" oosmo)
   (setq pt (getpoint "\nPick the center point.: "))
   (setvar "osmode" 0)
   (setq pp1 (polar pt 0 (+ (/ a 2.0) 5.0))
         pp2 (polar pp1 (dtr 180.0) (+ a 10.0))
         pp3 (polar pt (dtr 90.0) (+ (/ a 2.0) 5.0))
         pp4 (polar pp3 (dtr -90.0) (+ (+ (/ a 2.0) h) l 10.0))
         pt1 (polar pt (dtr -90.0) h)
         pt2 (polar pt1 0 (/ d 2.0))
         pt3 (polar pt2 (dtr 100.0) (/ thk (cos (dtr 10.0))))
         pt4 (polar pt (dtr -90.0) (/ b 2.0))
         pt5 (polar pt (dtr -90.0) (- h thk))
         pt6 (polar pt1 0 (/ g 2.0))
         pt7 (polar pt6 (dtr -90.0) e)
         pt8 (polar pt1 (dtr -90.0) e)
         pt9 (polar pt8 0 (/ ans 2.0))
        pt10 (polar pt9 (dtr -90.0) (- l (+ e k))) 
        pt11 (polar pt10 (dtr 180.0) (/ ans 2.0))     
        pt12 (polar pt11 (dtr 270.0) k)
        pt13 (polar pt11 (dtr 180.0) (/ ans 2.0))
        pt14 (polar pt1 (dtr 180.0) (/ d 2.0))
        pt15 (polar pt14 (dtr 80.0) (/ thk (sin (dtr 80.0))))
        pt16 (polar pt1 (dtr 180.0) (/ g 2.0))
        pt17 (polar pt (dtr -70.0) (/ a 2.0))
        pt18 (polar pt (dtr -35.0) (/ a 2.0))
        pt19 (polar pt (dtr 215.0) (/ a 2.0))
        pt20 (polar pt9 (dtr 180.0) of)
        pt21 (polar pt10 (dtr 180.0) of)) 
;  
  (command "circle" pt "d" b)
  (command "circle" pt "d" a)  (setq ena (entlast))
  (command "line" pp1 pp2 "")  (setq ena1 (entlast))
  (command "line" pp3 pp4 "")  (setq ena2 (entlast))
;
  (setq tbl (tblsearch "LAYER" "CEN-M")) 
  (if (null tbl)
  (command "layer" "n" "cen-m" "c" 1 "cen-m" "lt" "center2" "cen-m" "")
  )
  (command "chprop" ena1 ena2 "" "la" "cen-m" "")
  (command "line" pt1 pt2 pt3 pt4 ""
           "line" pt3 pt5 "")
  (setq ena3 (entnext ena2)
        ena4 (entnext ena3)
        ena5 (entnext ena4)
        ena6 (entnext ena5))
;
  (command "line" pt6 pt7 "")   (setq ena7 (entlast))
  (command "line" pt8 pt9 pt10 pt11 "")
  (setq ena8 (entnext ena7)
        ena9 (entnext ena8)
       ena10 (entnext ena9))
;  
  (command "mirror" ena3 ena4 ena5 ena6 ena7
                    ena8 ena9 ena10 "" pp3 pp4 "")
;
  (setq ena11 (entnext ena10)
        ena12 (entnext ena11)
        ena13 (entnext ena12)
        ena14 (entnext ena13)
        ena15 (entnext ena14)
        ena16 (entnext ena15)
        ena17 (entnext ena16)
        ena18 (entnext ena17))
;
  (command "line" pt6 pt2 "")   (setq ena19 (entlast))
  (command "line" pt14 pt16 "") (setq ena20 (entlast))
;
  (command "fillet" "r" r1
           "fillet" ena7 ena19
           "fillet" ena15 ena20
           "erase" ena19 ena20 ""
           "arc" pt13 pt12 pt10) (setq ena22 (entlast))
  
  (command "trim" ena4 ena12 "" pt17 ""  
           "line" pt20 pt21 "")  (setq ena21 (entlast))
; 
  (command "fillet" ena4 pt18
           "fillet" ena12 pt19
           "line" pt2 pt3 ""
           "line" pt14 pt15 ""
           "extend" ena22 "" pt21 ""
           "mirror" ena21 "" pt8 pt11 ""
           "redraw")
  (princ (strcat "\nYou select: " nd "  [Lifting capacity: " (rtos ld) "kg]" ))
)
);end if(ibolt)
;-----------------------------------------------------------------------------
(if (= an1 "P"); slot hole
  (progn
    (graphscr)
    (setq tbl (tblsearch "layer" "CEN-M")) 
    (if (null tbl)
    (command "layer" "n" "cen-m" "c" 1 "cen-m" "l" "center2" "cen-m" "")      
    )   
    (initget 1)
         (setvar "osmode" oosmo)
         (setq pt1 (getpoint "\nInsert center point: "))
         (setvar "osmode" 0)
         (setvar "blipmode" 0)
         (initget 7)
         (setq dia (getreal "\nSlot diameter : "))
         (setq rad (/ dia 2.0))
         (initget 7)
         (setq len (getreal "\nLength (cen to cen) : "))
         (setq ang 0)
         (setq pt2 (polar pt1 ang len))
         (setq kk (/ rad 4))
         (setq lenn (+ kk rad))
         (setq s1 (polar pt1 (+ ang (dtr 90)) rad)
            s2 (polar pt1 (+ ang (dtr 180)) rad)
            s3 (polar pt1 (+ ang (dtr 270)) rad)
            s4 (polar pt2 (+ ang (dtr 90)) rad)
            s5 (polar pt2 ang rad)
            s6 (polar pt2 (+ ang (dtr 270)) rad)
            k1 (polar pt1 (+ ang (dtr 90)) lenn)
            k2 (polar pt1 (+ ang (dtr 180)) lenn)
            k3 (polar pt1 (+ ang (dtr 270)) lenn)
            k4 (polar pt2 (+ ang (dtr 90)) lenn)
            k5 (polar pt2 ang lenn)
            k6 (polar pt2 (+ ang (dtr 270)) lenn))
       (command "line" s1 s4 "") (setq l1 (entlast))
       (command "line" s3 s6 "") (setq l2 (entlast)) 
       (command "arc" s1 s2 s3)  (setq ar1 (entlast)) 
       (command "arc" s4 s5 s6 ) (setq ar2 (entlast)) 
       (command "line" k1 k3 "") (setq c1 (entlast))
       (command "line" k4 k6 "") (setq c2 (entlast))                       
       (command "chprop" c1 c2 "" "la" "cen-m" "") 
       (command "_pedit" l1 "" "j" ar1 l2 ar2 "" "") (setq ful (entlast))
       (command "rotate" ful c1 c2 "" pt1 "\\")
 )
);end if -slot hole
;---------------------------------------------------------------------------
(if (= an1 "M");hand wheel elev.
  (progn   
    (setq ans (getint (strcat "\nHandle Outdia.(mm) <80/100/125/140/150/160/180/200/"
                                                   "225/250/300/350/400/450/500/600>: " )))
 (cond
 ((= ans 80)  (setq size '(80 14 33 30 15)))
 ((= ans 100) (setq size '(100 15 38 35 15)))
 ((= ans 125) (setq size '(125 15 45 38 15)))
 ((= ans 140) (setq size '(140 18 48 45 20)))
 ((= ans 150) (setq size '(150 18 50 46 20)))
 ((= ans 160) (setq size '(160 18 50 48 20)))
 ((= ans 180) (setq size '(180 20 50 50 25)))
 ((= ans 200) (setq size '(200 22 55 55 25)))
 ((= ans 225) (setq size '(225 22 55 55 25)))
 ((= ans 250) (setq size '(250 23 68 60 28)))
 ((= ans 300) (setq size '(300 24 68 70 28)))
 ((= ans 350) (setq size '(350 30 70 78 30))) 
 ((= ans 400) (setq size '(400 34 90 90 38)))
 ((= ans 450) (setq size '(450 35 95 100 40)))
 ((= ans 500) (setq size '(500 35 100 100 40)))
 ((= ans 600) (setq size '(600 40 100 105 42)))
 ) ;cond
;
 (setq dh  (nth 0 size) ;바깥직경 
        a  (nth 1 size) ;휠두께
        lh (nth 2 size) ;전높이
        db (nth 3 size) ;보스경
        f  (nth 4 size));리브높이@보스하단
;
 (setq lb (- lh a));보스높이
 (setq dc (- dh a));휠중심경
 (setq h (- lh a))    
    (graphscr) 
     (initget 1)
      (setvar "osmode" oosmo) 
      (setq pt1 (getpoint "\nInsert center point: "))
      (setvar "osmode" 0)
      (setq s1 (polar pt1 (dtr 90) lh)
            s2 (polar s1 (dtr 180) (/ dc 2.0))
	    c1 (polar s2 (dtr 270) (/ a 2.0))
	    c2 (polar c1 (dtr 0) dc)
            s3 (polar c1 (dtr 180) (/ a 2.0))
            s4 (polar s2 (dtr 270) a)
            s5 (polar s4 (dtr 0) dc)
	    s6 (polar c2 (dtr 0) (/ a 2.0))
            s7 (polar s2 (dtr 0) dc)
            s8 (polar pt1 (dtr 90) lb)
            s9 (polar s8 (dtr 180) (/ db 2.0)) 
	    s10 (polar pt1 (dtr 180) (/ db 2.0))
            s11 (polar s10 (dtr 0) db)
            s12 (polar s11 (dtr 90) lb) 
            s13 (polar s10 (dtr 90) f)
            s14 (polar s4 (dtr 0) (* a 0.8) ))            
       (command "line" s2 s7 "")   (setq e1 (entlast))
       (command "line" s4 s5 "")   (setq e2 (entlast)) 
       (command "arc" s2 s3 s4)    (setq e3 (entlast)) 
       (command "arc" s5 s6 s7)    (setq e4 (entlast)) 
       (command "line" s9 s10 "")  (setq e5 (entlast))
       (command "line" s10 s11 "") (setq e6 (entlast))
       (command "line" s11 s12 "") (setq e7 (entlast))
       (command "line" s13 s14 "") (setq e8 (entlast))    
       (command "mirror" e8 "" pt1 s1 "")   (setq e9 (entlast))       
       (command "rotate" e1 e2 e3 e4 e5 e6 e7 e8 e9 "" pt1 "\\")
       (princ (strcat "\nFC HANDWHEEL: <" (rtos dh) " dia>"))
 )   
);end if -handwheel elev.
;---------------------------------------------------------------------------
(if (= an1 "N");hand wheel plan
  (progn   
    (setq ans (getint (strcat "\nHandle Outdia.(mm) <80/100/125/140/150/160/180/200/"
                                                   "225/250/300/350/400/450/500/600>: " )))
 (cond
 ((= ans 80)  (setq size '(80 14 33 30 15)))
 ((= ans 100) (setq size '(100 15 38 35 15)))
 ((= ans 125) (setq size '(125 15 45 38 15)))
 ((= ans 140) (setq size '(140 18 48 45 20)))
 ((= ans 150) (setq size '(150 18 50 46 20)))
 ((= ans 160) (setq size '(160 18 50 48 20)))
 ((= ans 180) (setq size '(180 20 50 50 25)))
 ((= ans 200) (setq size '(200 22 55 55 25)))
 ((= ans 225) (setq size '(225 22 55 55 25)))
 ((= ans 250) (setq size '(250 23 68 60 28)))
 ((= ans 300) (setq size '(300 24 68 70 28)))
 ((= ans 350) (setq size '(350 30 70 78 30))) 
 ((= ans 400) (setq size '(400 34 90 90 38)))
 ((= ans 450) (setq size '(450 35 95 100 40)))
 ((= ans 500) (setq size '(500 35 100 100 40)))
 ((= ans 600) (setq size '(600 40 100 105 42)))
 ) ;cond
;
 (setq dh  (nth 0 size) ;바깥직경
        a  (nth 1 size) ;휠두께
        db (nth 3 size));보스경
; 
 (setq id (- dh (* 2.0 a)))   
    (graphscr) 
     (initget 1)
      (setvar "osmode" oosmo)
      (setq pt1 (getpoint "\nInsert center point: "))
      (setvar "osmode" 0)
      (setq s1 (polar pt1 (dtr 120) (/ db 2.0))
            s2 (polar pt1 (dtr 95) (/ id 2.0))
	    s3 (polar pt1 (dtr 60) (/ db 2.0))
	    s4 (polar pt1 (dtr 85) (/ id 2.0)))
      (command "circle" pt1 "d" dh)
      (command "circle" pt1 "d" id)
      (command "circle" pt1 "d" db)
      (command "line" s1 s2 "")   (setq e1 (entlast))
      (command "line" s3 s4 "")   (setq e2 (entlast))
       (if (>= ans 300)
	 (setq n 5)
	 (setq n 4)
       )	 
      (command "array" e1 e2  "" "p" pt1 n "" "")
      (princ (strcat "\nFC HANDWHEEL: <" (rtos dh) " dia>"))
 )   
);end if -handwheel plan
;---------------------------------------------------------------------------
(if (= an1 "Q");handgrip elev.
  (progn   
    (setq d (getreal "\n RoundBar Dia.(mm): " ))
    (setq w (getreal "\n Inside Width (mm): " ))
    (setq h (getreal "\nInside Height (mm): " ))    
    (setq r (* d 1.5))
    (graphscr) 
     (initget 1)
      (setvar "osmode" oosmo)
      (setq pt1 (getpoint "\npick the point: "))
      (setvar "osmode" 0)
      (setq s1 (polar pt1 (dtr 90) (/ d 2.0))
            s2 (polar s1 (dtr 180) h)
	    s3 (polar s2 (dtr 90) w)
	    s4 (polar s3 (dtr 0) h)
            s5 (polar s4 (dtr 90) d)	    
            s6 (polar s1 (dtr 270) d)
	    s7 (polar s1 (dtr 180) (+ h d)));offset point 
      (command "line" s1 s2 "")   (setq e1 (entlast))
      (command "line" s2 s3 "")   (setq e2 (entlast))
      (command "line" s3 s4 "")   (setq e3 (entlast))
      (command "line" s4 s5 "")   (setq e4 (entlast))
      (command "line" s1 s6 "")   (setq e5 (entlast))
      (command "fillet" "r" r)
      (command "fillet" e1 e2)    (setq e6 (entlast))
      (command "fillet" e2 e3)    (setq e7 (entlast))
      (command "_pedit" e1 "" "j" e2 e3 e6 e7 "" "") (setq e8 (entlast))
      (command "offset" d e8 s7 "")(setq e9 (entlast))    
      (command "_pedit" e4 "" "j" e8 e9 e5 "" "") (setq e10 (entlast))
      (command "rotate" e10 "" pt1 "\\")     
      (princ (strcat "\nround bar grip: <" (rtos d) " dia>"))
 )   
);end if -handgrip elev.
;---------------------------------------------------------------------------
(if (= an1 "R");handgrip plan
  (progn   
    (setq d (getreal "\nRoundBar Dia.(mm): " ))
    (setq w (getreal "\nInside Width (mm): " ))    
    (graphscr) 
     (initget 1)
      (setvar "osmode" oosmo)
      (setq pt1 (getpoint "\npick the point: "))
      (setvar "osmode" 0)
      (setq s1 (polar pt1 (dtr 90) (+ (/ w 2.0) (/ d 2.0)))
            s2 (polar s1 (dtr 0) (/ d 2.0))
	    s3 (polar s2 (dtr 180) d)
	    s4 (polar s1 (dtr 90) (/ d 2.0))
            s5 (polar s1 (dtr 270) (+ w d))	    
            s6 (polar s5 (dtr 0) (/ d 2.0))
	    s7 (polar s6 (dtr 180) d)
            s8 (polar s5 (dtr 270) (/ d 2.0)))
      (command "line" s3 s7 "")   (setq e1 (entlast))
      (command "line" s2 s6 "")   (setq e2 (entlast))
      (command "arc" s2 s4 s3)    (setq e3 (entlast)) 
      (command "arc" s7 s8 s6)    (setq e4 (entlast))      
      (command "_pedit" e1 "" "j" e2 e3 e4 "" "") (setq e5 (entlast))      
      (command "rotate" e5 "" pt1 "\\")     
      (princ (strcat "\nround bar grip: <" (rtos d) " dia>"))
 )   
);end if -handgrip plan
;---------------------------------------------------------------------------
(if (or (= an1 "S") (= an1 "T"));PT TAP
 (progn
  (textpage)
 (princ "\n                                PT TAP                                        ")
 (princ "\n============================================================================= ")
 (princ "\n  [1]  6A(1/8)=10.5     [2]  8A(1/4)=13.8     [3] 10A(3/8)=17.3         ")          
 (princ "\n  [4] 15A(1/2)=21.7     [5] 20A(3/4)=27.2     [6] 25A(1)=34             ")
 (princ "\n  [7] 32A(1 1/4)=42.7   [8] 40A(1 1/2)=48.6   [9] 50A(2)=60.5           ")
 (princ "\n [10] 65A(2 1/2)=76.3  [11] 80A(3)=89.1      [12] 90A(3 1/2)=101.6      ")
 (princ "\n [13]100A(4)=114.3     [14]125A(5)=139.8     [15]150A(6)=165.2          ")
 (princ "\n [16]175A(7)=190.7     [17]200A(8)=216.3     [18]225A(9)=241.8          ")
 (princ "\n [19]250A(10)=267.4    [20]300A(12)=318.5                               ") 
 (princ "\n============================================================================= ")    
 (setq ans (getint"\nselect no.: " ))
 (cond
 ((= ans 1)  (setq size '("1/8"   9.7280 8.5660  6.2   7.4 28)))
 ((= ans 2)  (setq size '("1/4"   13.157 11.445  9.4  11.0 19)))
 ((= ans 3)  (setq size '("3/8"   16.662 14.950  9.7  11.4 19)))
 ((= ans 4)  (setq size '("1/2"   20.955 18.631  12.7 15.0 14)))
 ((= ans 5)  (setq size '("3/4"   26.441 24.117  14.1 16.3 14)))
 ((= ans 6)  (setq size '("1"     33.249 30.291  16.2 19.0 11)))
 ((= ans 7)  (setq size '("1 1/4" 41.910 38.951  18.5 21.4 11)))
 ((= ans 8)  (setq size '("1 1/2" 47.803 44.845  18.5 21.4 11)))
 ((= ans 9)  (setq size '("2"     59.614 56.656  22.8 25.7 11)))
 ((= ans 10) (setq size '("2 1/1" 75.184 72.226  26.7 30.2 11)))
 ((= ans 11) (setq size '("3"     87.884 84.926  29.9 33.3 11)))
 ((= ans 12) (setq size '("3 1/2" 100.33 97.372  31.5 34.9 11))) 
 ((= ans 13) (setq size '("4"     113.03 110.072 35.8 39.3 11)))
 ((= ans 14) (setq size '("5"     138.43 135.472 40.1 43.6 11)))
 ((= ans 15) (setq size '("6"     163.83 160.872 40.1 43.6 11)))
 ((= ans 16) (setq size '("7"     189.23 186.272 48.9 54.0 11)))
 ((= ans 17) (setq size '("8"     214.63 211.672 52.1 57.2 11)))
 ((= ans 18) (setq size '("9"     240.03 237.072 52.1 57.2 11)))
 ((= ans 19) (setq size '("10"    265.43 262.472 55.2 60.3 11)))
 ((= ans 20) (setq size '("12"    316.23 313.272 58.7 65.1 11))) 
 ) ;cond
;
 (setq nd  (nth 0 size) ;호칭경
        do (nth 1 size) ;골지름
        di (nth 2 size) ;내경
        lt (nth 3 size) ;pt길이
        lf (nth 4 size) ;pf길이
        tn (nth 5 size)) ;나사산수
 (setq the 1.7833);taper각
 (setq gh (/ (- do di) 2.0));골깊이
 (setq t1 (- (dtr 270.0) (dtr the)));아래각도-r
 (setq t2 (+ (dtr 270.0) (dtr the)));아래각도-l
 (setq dl (/ lt (cos (dtr the))));외경 실장
;
 (if (= an1 "S");pt elev.
  (progn 
   (graphscr) 
    (initget 1)
    (setvar "osmode" oosmo)
    (setq pt1 (getpoint "\nInsert center point: "))
    (setvar "osmode" 0)
    (setq p1 (polar pt1 (dtr 0) (/ do 2.0)))
    (setq p2 (polar p1 t1 dl))
    (setq p3 (polar p1 (dtr 180.0) do))
    (setq p4 (polar p3 t2 dl))
    (setq p5 (polar pt1 (dtr 90) 3.0));cen.line
    (setq p6 (polar p5 (dtr 270) (+ lt 6.0)));cen.line

    (setq p7 (polar p1 (dtr 180) gh));
    (setq p8 (polar p7 t1 dl));
    (setq p9 (polar p3 (dtr 0) gh));
    (setq p10 (polar p9 t2 dl));
   
    (command "line" p1 p3 "")   (setq e1 (entlast))
    (command "line" p2 p4 "")   (setq e2 (entlast))
    (command "line" p1 p2 "")   (setq e3 (entlast))
    (command "line" p3 p4 "")   (setq e4 (entlast))

    (command "line" p7 p8 "")    (setq e11 (entlast));
    (command "line" p9 p10 "")   (setq e12 (entlast));
    
    (command "chamfer" "d" gh gh) 
    (command "chamfer" e3 e2)     (setq e7 (entlast))
    (setq eg1 (entget e7))
    (setq ep1 (cdr (assoc 10 eg1)))
    (command "chamfer" e4 e2)     (setq e8 (entlast))
    (setq eg2 (entget e8))
    (setq ep2 (cdr (assoc 10 eg2)))
    (command "line" ep1 ep2 "")   (setq e9 (entlast))
    (command "line" p5 p6 "")     (setq e10 (entlast))
    (command "chprop" e10 "" "la" "cen-m" "")
    (command "rotate" e1 e2 e3 e4 e7 e8 e9 e10 e11 e12 "" pt1 "\\");e5 e6
    (princ (strcat "\n최소깊이/적용호칭:PT " nd "(나사산수:" (rtos tn) "/inch)"))
    (princ (strcat "\nPF의 최소깊이: " (rtos lf 2 2) ))
   )
  );pt elev.
 (if (= an1 "T");pt plan
  (progn 
   (graphscr) 
    (setq pt1 (getpoint "\nInsert center point: "))         
    (setq p1 (polar pt1 (dtr 0) (/ do 2.0)))
    (setq p2 (polar pt1 (dtr 90) (+ 3.0 (/ do 2.0))))
    (setq p3 (polar p2 (dtr 270) (+ 6.0 do)))
    (setq p4 (polar pt1 (dtr 180) (+ 3.0 (/ do 2.0))))
    (setq p5 (polar p4 (dtr 0) (+ 6.0 do)))
    (command "circle" pt1 "d" di)
    (command "arc" "c" pt1 p1 "a" 270)
    (command "line" p2 p3 "") (setq e1 (entlast))
    (command "line" p4 p5 "") (setq e2 (entlast))
    (command "chprop" e1 e2 "" "la" "cen-m" "")
    (princ (strcat "\n적용호칭<PT" nd ">나사산수:" (rtos tn) "/inch"))
  )
 );pt plan 
 )	    
);PT tap  
;-------------------------------------------------------------------------------------------
(if (= an1 "U");WASHER
 (progn
  (princ "\nNot use the size in () as far as possible")  
  (initget 7)
  (setq an2 (getreal "\nBolt diameter(M); <2,3,5,6,8,10,12,(14),16,(18),20,(22),24,30,36>: " )) 
(cond
((= an2 2) (setq size '(2.2  4.3  0.5  2.1  4.4  0.5 1.6  4.6))) 
((= an2 3) (setq size '(3.2  6.0  0.5  3.1  5.9  0.6 2.4  6.4))) 
((= an2 5) (setq size '(5.3  10   1.0  5.1  9.2  1.3 4.0  9.2))) 
((= an2 6) (setq size '(6.6  12.5 1.6  6.1  12.2 1.5 5.0  11.5))) 
((= an2 8) (setq size '(9.0  17.0 1.6  8.2  15.4 2.0 6.5  15.0))) 
((= an2 10)(setq size '(11   21   2.0  10.2 18.4 2.5 8.0  19.6)))
((= an2 12)(setq size '(14   24   2.3  12.2 21.5 3.0 10.0 21.9))) 
((= an2 14)(setq size '(16   28   3.2  14.2 24.5 3.5 11   25.4))) 
((= an2 16)(setq size '(18   30   3.2  16.2 28.0 4.0 13   27.7))) 
((= an2 18)(setq size '(20   34   3.2  18.2 31.0 4.6 15   31.2))) 
((= an2 20)(setq size '(22   37   3.2  20.2 33.8 5.1 16   34.6))) 
((= an2 22)(setq size '(24   39   3.2  22.5 37.7 5.6 18   37)))
((= an2 24)(setq size '(26   44   4.5  24.5 40.3 5.9 19   41.6))) 
((= an2 30)(setq size '(33   56   4.5  30.5 49.9 7.5 24   53.1)))
((= an2 36)(setq size '(39   66   6.0  36.5 59.1 9.0 29   63.5)))
)
(setq nd (nth 0 size) ;p/w id
      do (nth 1 size) ;p/w od
      di (nth 2 size) ;p/w thk
      lt (nth 3 size) ;s/w id
      lf (nth 4 size) ;s/w od
      tn (nth 5 size) ;s/w thk
      n1 (nth 6 size) ;nut height
      n2 (nth 7 size));nut od
(textpage)
(princ "\n-------------------------------------------------")
(princ (strcat "\nPlain  Washer: (" "I.D:" (rtos nd 2 1) "    O.D:" (rtos do 2 1) "    Thk:" (rtos di 2 1) ")"))
(princ (strcat "\nSpring Washer: (" "I.D:" (rtos lt 2 1) "    O.D:" (rtos lf 2 1) "    Thk:" (rtos tn 2 1)")"))
(princ (strcat "\nNut: (" "O.D:" (rtos n2 2 1) "    Height:" (rtos n1 2 1)")"))
(princ (strcat "\nHeight (P/W+S/W): " (rtos (+ di tn) 2 1)))
(princ (strcat "\nHeight (NUT+P/W+S/W): " (rtos (+ di tn n1) 2 1)))
(princ "\n-------------------------------------------------")
(princ "\n-press anykey for exit-")
  (grread)    
  (graphscr)
 )
);WASHER
;-----------------------------------------------------------------------------------
(command "undo" "e")   
(setvar "cmdecho" ocmde)                               
(setvar "osmode" oosmo)
(setvar "peditaccept" oed)  
(setq *error* olderr)  
(princ)                                               
)
;;;;;;;;;;;;;;;;;;;;;;
(princ "\n\tHEX.BOLT&NUT/TAP/WRENCH BOLT/STOP BOLT/ETC.[typing BOLT] : 1999.6 / by K.W.PARK")
(princ)

