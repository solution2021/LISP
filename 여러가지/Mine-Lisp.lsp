;-------------------------------------------------mine lisp-----------------

;******************* Layer_Set,Line **********************************************

(defun C:1()(Command "-layer" "s" "CEN" "")(princ "\n Layer = CEN")(prin1))
(defun C:11()(Command "-layer" "s" "WAL01" "")(princ "\n Layer = WAL01")(prin1))

(defun C:2()(Command "-layer" "s" "WAL01" "")(princ "\n Layer = WAL01")(prin1))
(defun C:22()(Command "-layer" "s" "HD" "")(princ "\n Layer = HD")(prin1))

(defun C:3()(Command "-layer" "s" "WAL02" "")(princ "\n Layer = WAL02")(prin1))
(defun C:33()(Command "-layer" "s" "OFF" "")(princ "\n Layer = OFF")(prin1))

(defun C:4()(Command "-layer" "s" "TXT" "")(princ "\n Layer = TXT")(prin1))
(defun C:44()(Command "-layer" "s" "Y04" "")(princ "\n Layer = Y04")(prin1))

(defun C:5()(Command "-layer" "s" "DIM" "")(princ "\n Layer = DIM")(prin1))
(defun C:55()(Command "-layer" "s" "Y05" "")(princ "\n Layer = Y05")(prin1))
             
(defun C:6()(Command "-layer" "s" "COR" "")(princ "\n Layer = COR")(prin1))
(defun C:66()(Command "-layer" "s" "ETC" "")(princ "\n Layer = ETC")(prin1))

(defun C:7()(Command "-layer" "s" "HAT" "")(princ "\n Layer = HAT")(prin1))
(defun C:77()(Command "-layer" "s" "FIN" "")(princ "\n Layer = FIN")(prin1))

(defun C:8()(Command "-layer" "s" "WID" "")(princ "\n Layer = WID")(prin1))
(defun C:88()(Command "-layer" "s" "SOL" "")(princ "\n Layer = SOL")(prin1))

(defun C:9()(Command "-layer" "s" "FUR" "")(princ "\n Layer = FUR")(prin1))
(defun C:99()(Command "-layer" "s" "ELE" "")(princ "\n Layer = ELE")(prin1))

(defun C:0()(Command "-layer" "s" "0" "")(princ "\n Layer = 0")(prin1))
(defun C:00()(Command "-layer" "s" "HIDE" "")(princ "\n Layer = HIDE")(prin1))             



;******************** 2p Circle = 2c ****************************************

(defun c:2c()(Command "circle" "2p" "")(Princ "\n 2P Circle")(prin1))



;******************** Break_2Point = B ************************************************
(defun C:B(/ ss sno pt1 pt2 no x)
       (setvar "cmdecho" 0)
       (princ " = BREAK2P")
       (setq ss (ssget))
       (setq sno (sslength ss))
       (setq pt1 (getpoint "\nEnter 1st Point : "))
       (setq pt2 (getpoint pt1 "\nEnter 2nd Point : "))
       (setq no 0)
       (repeat sno
           (setq x (ssname ss no))
           (command "break" x pt1 pt2)
           (setq no (1+ no))
       )(prin1)
)




;******************** Offset to Clayer = OO *************************
(defun C:OO(/ k pt ss Q:LA)
       (setvar "cmdecho" 0)
       (princ " = OFFSET & CHANGE LAYER TO CURRENT..")
       (setq Q:LA (getvar "clayer"))
       (if Q:ODIST
           (progn
               (prompt "\nOffset distance or Through <")
               (princ Q:ODIST)
               (princ "> : ")
               (setq k (getdist))
               (if k
                   (setq Q:ODIST k)
               )
           )
           (progn
               (prompt "\nOffset distance or Through <Through> : ")
               (initget (+ 1 2 4))
               (setq Q:ODIST (getdist))
           )
       )
       (setq ss (entsel "\nSelect object to offset : "))
       (while ss
              (setq pt (getpoint "\nSide to offset? "))
              (command "offset" (setq z Q:ODIST) 
                       (setq k (cadr ss)) (setq k pt) "")
              (command "change" "L" "" "P" "la" (setq k Q:LA) "")
              (setq ss (entsel "\nSelect object to offset : "))
       )
       (setvar "cmdecho" 1)
       (prin1)
)

;******************* Area : m2 (py) = AR *******************************
    (defun c:ar ( / doc space ss index obj mi mx minpt maxpt inspt area txtobj )
 (setvar "cmdecho" 0)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (cond
    ((= (vla-get-activespace doc) 1) (setq space (vla-get-modelspace doc)))
    ((= (vla-get-activespace doc) 0) (setq space (vla-get-paperspace doc)))
    )
  (if (setq ss (ssget (list (cons 0 "CIRCLE,ARC,*POLYLINE,LINE,ELLIPSE,SPLINE"))))
    (if (or (setq ts (getreal (strcat "\n텍스트 높이 입력 <" (vl-princ-to-string (getvar "textsize")) ">:"))) (setq ts (getvar "textsize")))
      (progn
  (setvar "textsize" ts)
 (setq index 0)
 (repeat (sslength ss)
   (setq obj (vlax-ename->vla-object (ssname ss index)))
   (vla-getboundingbox obj 'mi 'mx)
   (setq minpt (vlax-safearray->list mi) maxpt (vlax-safearray->list mx))
   (setq inspt (polar minpt (angle minpt maxpt) (/ (distance minpt maxpt) 2)))
   (setq inspt (list (car inspt) (+ (cadr inspt) (* ts 2))))
   (setq area (vla-get-area obj) len (vlax-curve-getdistatparam obj (vlax-curve-getendparam obj)))
   (mapcar '(lambda (a b)
       (setq p (vlax-3d-point inspt))
       (setq txtobj (vla-addtext space (strcat a b) p ts))
       (vla-put-alignment txtobj 4)
       (vla-put-textalignmentpoint txtobj p)
       (setq inspt (polar inspt (/ (* 270 pi) 180) (* ts 2)))
       )
              '("평 : " "면적(㎡)  : " "길이(mm) : " ) (list (rtos (/ area 3305796.) 2 2) (rtos (/ area 1000000.) 2 3) (rtos len 2 2))
    )
   (setq index (1+ index))
   )
 (command "chprop" (ssget "x" '((0 . "TEXT") (1 . "평*"))) "" "c" "7" "")
 )
      )
    )
  (princ)
  )
(vl-load-com)
(prompt "\n[ AAA ]")
(princ)


;***************** Rotate Copy = RC  ***********************************
(DEFUN C:RC (/ z e ang pt1 pt2)
       (SETVAR "CMDECHO" 0)
       (SETQ E (ssget))
       (setq pt1 (getpoint "\nBase Point : "))
       (setq ang (getangle pt1 "\nRotate Angle : "))
       (setq pt2 (getpoint pt1 "\nDisplacement : "))
       (SETVAR "BLIPMODE" 0)
       (setq ang (angtos ang 0 2))
       (command "copy" (setq z e) "" (setq z pt1) (setq z pt1))
       (command "rotate" (setq z e) "" (setq z pt1) (setq z ang))
       (command "move" (setq z e) "" (setq z pt1) (setq z pt2))
       (redraw)
       (SETVAR "CMDECHO" 1)
       (SETVAR "BLIPMODE" 1)
       (PRIN1)
)

;***************** Layer Enitiy Erase = LE ***************************
(defun C:LE (/ A L S)
  (princ "\nSelect entity to erase layer : ")
  (setq
    A (ssget)
    L (cdr (assoc 8 (entget
      (ssname A 0))))
    S (ssget "X" (list (cons 8 L))))
  (command "ERASE" S ""))


;**************** Entity Current Layer  = LC ************************
(defun c:LC (/ cmd ss)
     (setq cmd (getvar "cmdecho"))
     (setvar "cmdecho" 0)
     (setq ss (ssget))
     (command "change" ss "" "p" "la" (getvar "clayer") "")
 (prompt "\nChange is done.")
     (setvar "cmdecho" cmd)
     (prin1)
)

;***************** Layer ON ( all ) = LO ***************************
(defun c:LO ()
  (setvar "cmdecho" 0) (command "layer" "on" "*" "") 
  (prompt "\nAll Layer is ON.")
  (prin1))

;***************** Layer Thaw (all) = LOO ****************************
(defun c:LOO ()
  (setvar "cmdecho" 0) (command "layer" "Thaw" "*" "")
  (prompt "\nAll Layer is Thaw.")
  (prin1))

;***************** Layer LOCK_OFF ( Select ) = LU ***************************
(defun c:LU (/ cmd ename elist lyr)
    (setvar "cmdecho" 0) 
  (setq e (car (entsel "\nPick an object to be UNLOCK LAYER :")))
  (if e (progn 
    (setq e (entget e)) 
    (setq n (cdr (assoc 8 e))) 
    (command "layer" "U" n "")))
    (prompt "\nLayer is Lock Off.")
    (prin1))

;***************** Layer LOCK_OFF ( All )  = LUU ***************************
(defun c:LUU ()
  (setvar "cmdecho" 0) (command "layer" "U" "*" "")
  (prompt "\nAll Layer is Lock off.")
  (prin1))

;****************** Entity Layer_Set  = LS ********************************
(defun c:LS (/ cmd ename elist lyr)
     (setq cmd (getvar "cmdecho"))
     (setvar "cmdecho" 0) 
     (setq ename (car (entsel "\n Select an entity :")))
     (while (null ename)
            (setq ename (car (entsel "\n Select again :")))
     )
     (setq elist (entget ename))
     (setq lyr (assoc 8 elist))
     (command "layer" "set" (cdr lyr) "")
     (prompt (strcat "\n Now the current layer is " (cdr lyr) "."))
     (setvar "cmdecho" cmd)
     (prin1)
)
;**************** Entity Layer_Set_OFF  = LF ****************************
(defun c:LF (/ cmd ename elist lyr)
     (setvar "cmdecho" 0) 
  (setq e (car (entsel "\nPick an object to be OFF LAYER :")))
  (if e (progn 
    (setq e (entget e)) 
    (setq n (cdr (assoc 8 e))) 
    (command "layer" "off" n "")))
  (prin1)
)
;**************** Entity Layer_Set_LOCK  = LK ****************************
(defun c:LK (/ cmd ename elist lyr)
     (setvar "cmdecho" 0) 
  (setq e (car (entsel "\nPick an object to be LOCK LAYER :")))
  (if e (progn 
    (setq e (entget e)) 
    (setq n (cdr (assoc 8 e))) 
    (command "layer" "LO" n "")))
  (prin1)
)
;**************** Entity Layer_set_Freeze = LFF ****************************
(defun c:LFF (/ cmd ename elist lyr)
  (setvar "cmdecho" 0) 
  (setq e (car (entsel "\nPick an object to be OFF LAYER :")))
  (if e (progn 
    (setq e (entget e)) 
    (setq n (cdr (assoc 8 e))) 
    (command "layer" "Freeze" n "")))
  (prin1))


;MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM


;*********************** ZOOM = "EXTEND" ****************************
(defun C:ZE()(Command "ZOOM" "E")(princ "\n zoom_EXTEND")(prin1))

;*********************** ZOOM = "DYNAMIC" ****************************
(defun C:ZD()(Command "ZOOM" "D")(princ "\n zoom_DYNAMIC")(prin1))

;*********************** DIM = "VER" **********************************
(defun c:DV()(setvar "osmode" 33)(setvar "cmdecho" 0)
             (Command "-layer" "s" "DIM" "")(command "dim" "vert")
             (princ "\n Layer=DIM Command=DIM_VERT")(prin1))

;*********************** DIM = "HOR" **********************************
(defun c:DH()(setvar "osmode" 33)(setvar "cmdecho" 0)
             (Command "-layer" "s" "DIM" "")(command "dim" "horiz")
             (princ "\n Layer=DIM Command=DIM_HORIZ")(prin1))

;*********************** DIM = "ALI" **********************************
(defun c:DA()(setvar "osmode" 33)(setvar "cmdecho" 0)
             (Command "-layer" "s" "DIM" "")(command "dim" "ali")
             (princ "\n Layer=DIM Command=DIM_ALIGEND")(prin1))

;*********************** DIM = "RADI" **********************************
(defun c:DR()(setvar "osmode" 33)(setvar "cmdecho" 0)
             (Command "-layer" "s" "DIM" "")(command "dim" "radius")
             (princ "\n Layer=DIM Command=DIM_RADIUS")(prin1))

;*********************** DIM = "ANGULAR" *******************************
(defun c:DG()(setvar "osmode" 33)(setvar "cmdecho" 0)
             (Command "-layer" "s" "DIM" "")(command "_dim" "angular")
             (princ "\n Layer=DIM Command=DIM_ANGULAR")(prin1))

;*********************** DIM = "BASE" **********************************
(defun c:DB()(setvar "osmode" 33)(setvar "cmdecho" 0)
             (Command "-layer" "s" "DIM" "")(command "dim" "base")
             (princ "\n Layer=DIM Command=DIM_BASELINE")(prin1))

;*********************** DIM = "LINEAR" **********************************
(defun c:DN()(setvar "osmode" 33)(setvar "cmdecho" 0)
             (Command "-layer" "s" "DIM" "")(command "_dimlinear")
             (princ "\n Layer=DIM Command=DIM_LINEAR")(prin1))



;********************** DIM= "UPDATE" **********************************
(defun C:DU()(setvar "cmdecho" 0)(COMMAND "-DIMSTYLE" "A" )(prin1))


;*********************** XLINE = "VER" *********************************
(defun c:XV()(setvar "osmode" 167)(setvar "cmdecho" 0)
             (command "XLINE" "VER")
             (princ "\n Command=XLINE_VER")(prin1))


;*********************** XLINE = "HOR" *********************************
(defun c:XH()(setvar "osmode" 167)(setvar "cmdecho" 0)
             (command "XLINE" "HOR")
             (princ "\n Command=XLINE_HOR")(prin1))


;********************** Entity UCS ************************************
(defun C:UU()(setvar "cmdecho" 0)(COMMAND "UCS" "W" "UCS" "E")(prin1))

;********************** UCS ORIGINAL ************************************
(defun C:UO()(setvar "cmdecho" 0)(COMMAND "UCS" "W" "UCS" "O")(prin1))

;********************** Fillet = 0 ************************************
(defun c:FF()(command "fillet" "r" "0")(prin1)(command "fillet")(prin1))

;********************** Fillet = r ************************************
(defun c:FR()(command "fillet" "r"))

;************************* Polyline Edit = PW ***************************
(defun c:pw(/ ss pw n k)
   (prompt "\n Select Poly line")
   (setq ss (ssget))
   (if (= cdwd nil) (setq cdwd 0))
   (setq pw (getreal (strcat "\n Enter Poly line Width <" (rtos cdwd 2 0) ">:")))
   (if (= pw nil) (setq pw cdwd))
   (setq n (sslength ss))
   (setq k 0)
   (while (<= 1 n)
      (setq en (ssname ss k))
      (if (eq (cdr (assoc 0 (entget en))) "LWPOLYLINE")(progn
          (command "pedit" en "w" pw "")
      )   )
      (if (eq (cdr (assoc 0 (entget en))) "LINE")(progn
          (command "pedit" en "y" "w" pw "")
      )   )
      (setq n (- n 1) k (+ k 1))
   )
   (setq cdwd pw)
   (prin1)
)



;************************* Polyline Edit = PJ ***************************
(defun c:pj(/ ss en ent)
   (prompt "\n Select Join Poly line")
   (setq ss (ssget))
   (setq en (ssname ss 0))
   (setq ent (cdr (assoc 0 (entget en))))
   (if (= ent "LINE")(command "pedit" en "y" "j" ss "" ""))
   (if (= ent "LWPOLYLINE")(command "pedit" en "j" ss "" ""))
   (command "select" "l" pause)
   (prin1)
)


;************************* OSANP-SET = SS ***************************
;0-NONE /64-INSertion /1-ENDpoint /128-PERpendicular 
;2-MIDpoint /256-Tangent /4-CENter /512-NEArest /32-INTersection
;8-NODe /1025-QUIck /16-QUAdrant /2048-APParent Intersection   
;----------------------------------------------------------------

(defun c:ss()(command "osmode" "167")
             (prompt "\nOSMODE=END INS INT MID CEN PER")(prin1))



;*********************** INSULATION DRAW = ISD ****************************
(defun c:ISD(/ myerror os bl cl pt1 pt2 pt3 pt4 thi angX angY n di
              b1 b2 b3 b4 p1 p2 p3 p4 p5 p6 p7 p8 f1 f2)
   (setvar "cmdecho" 0)
   (setq os (getvar "osmode"))
   (setq bl (getvar "blipmode"))
   (setq cl (getvar "clayer"))
;----------------- Internal error handler -------------------
 (defun myerror(S)
  (if (/= s "Function cancelled")
      (princ (strcat "\nError:" s))
  )
  (setq *error* olderr)
  (setvar "osmode" os)
  (setvar "clayer" cl)
  (setvar "blipmode" bl)
  (princ)
 )
 (setq olderr *error* *error* myerror)
;------------------------------------------------------------
   (setvar "blipmode" 0) (setvar "osmode" 33)
   (setq pt1 (getpoint "\n Select Drawing Start end<int> of "))
   (setvar "osmode" 128)
   (setq pt2 (getpoint pt1 "\n Insulation With per of "))
   (setvar "osmode" 129)
   (setq pt3 (getpoint pt1 "\n Select Drawing end end<per> of "))
   (setq pt4 pt1)
   (setvar "blipmode" 0)(setvar "osmode" 0)
   (if (= th nil) (setq th 50))
   (setq thi (getdist (strcat "\n Enter Insulation With <"(rtos th 2 0)">:")))
   (if (= thi nil) (setq thi th))
   (setq sc (/ thi 50))
   (setq angX (angle pt1 pt3) angY (angle pt1 pt2))
   (setq di (distance pt1 pt3))
   (setq n (/ di (* sc 25)) )
;   (laset "etc")
   (setq b1 (polar pt1 angY (* sc 4.32))
         b2 (polar pt1 angY (* sc 13.55))
         b3 (polar pt1 angY (* sc 36.45))
         b4 (polar pt1 angY (* sc 50.00))) 
   (setq p1 (polar b1 angX (* sc 8.23))
         p2 (polar b2 angX (* sc 9.35))
         p3 (polar b3 angX (* sc 3.15))
         p4 (polar b4 angX (* sc 12.50))
         p5 (polar b3 angX (* sc 21.85))
         p6 (polar b2 angX (* sc 15.65))
         p7 (polar b1 angX (* sc 16.77))
         p8 (polar pt1 angX (* sc 25.00)))
   (command "pline" pt1 "a" "s" p1  p2 "l" p3 "a" "s" p4
                    p5 "l" p6 "a" "s" p7 p8 "")
   (while (>= n 1)
     (setq pt1 p8) 
     (setq p8 (polar pt1 angX (* sc 25.00)))
     (command "copy" "l" "" pt1 p8)
     (setq n (- n 1))
   );while end
   (if (= nm nil) (setq nm "N"))
   (setq t1 (strcat "\n Pline Join<" nm ">?:"))
   (setq nmm (getstring t1))
   (if (= nmm "") (setq nmm nm))
   (setq nmm (strcase nmm))
   (if (= nmm "Y") (progn
       (setq f1 (polar pt4 angY sc))
       (setq f2 (polar pt3 angY sc))
       (command "pedit" "l" "j" "f" f1 f2 "" "" "")
    ));if end
   (setq th thi nm nmm)
   (setvar "clayer" cl) (setvar "blipmode" bl)
   (setvar "osmode" os)
   (prin1)
)





;************************* SLASH DRAW-Simple Line = SLD ***************************
(defun c:SLD(/ myerror cl os or sc pt1 pt2 a1 a2 a3 a4 d-h ptm p1 p2 p3 p4)
;start --- Internal error handler ----------
 (defun myerror(S)
 (if (/= s "Function cancelled")(princ (strcat "\nError:" s)))
 (setvar "clayer" cl) (setvar "osmode" os)
 (setvar "orthomode" or) (setvar "blipmode" bl)
 (setq *error* olderr)(princ) )
 (setq olderr *error* *error* myerror)
;end----------------------------------------
   (setq cl (getvar "clayer") os (getvar "osmode")
         or (getvar "orthomode") bl (getvar "blipmode") )
   (setvar "orthomode" 0)(setvar "osmode" 0) (setvar "blipmode" 0)
   (setq lts (getvar "ltscale"))
   (prompt "\nScale factor<") (prin1 lts) (prompt ">:")
   (setq dsc (getint))
   (if (= dsc nil) (setq dsc lts))
   (setq pt1 (getpoint "\nStart point of section mark->"))
   (setq pt2 (getpoint pt1 "\nEnd point of section mark->"))
;   (laset "a-note-symb-lgt")
   (setq a1 (angle pt1 pt2) a2 (+ a1 (/ pi 2))
         a3 (+ a1 pi)       a4 (+ a2 pi))
   (setq d-h (/ (distance pt1 pt2) 2))
   (setq ptm (polar pt1 a1 d-h))
   (setq p1 (polar ptm a3 (* dsc 1.5))
         p2 (polar p1 a4 (* dsc 1.5))
         p3 (polar ptm a1 (* dsc 1.5))
         p4 (polar p3 a2 (* dsc 1.5)))
   (command "pline" pt1 p1 p2 p4 p3 pt2 "")
   (setvar "clayer" cl)(setvar "osmode" os)
   (setvar "orthomode" or)(setvar "blipmode" bl)
   (prin1)
)





;************************ WALL DRAW = WAD ***********************************************
;;====================================================
; WALL DRAW(93cho_i)
;;----------------------------------------------------
(defun c:wad (/ cl os d1 d2 d3 cl st1 e1
               p1 p2 p3 p4 ang1 ang2 ang3)
   (setq cl (getvar "clayer") os (getvar "osmode"))
   (setvar "osmode" 0)
   (setq d2 200)
   (if (/= wallthk nil)(setq d2 wallthk) )
   (setq st1 (itoa d2))
   (setq st1 (strcat "Enter wall thikness<" st1 ">:"))
   (prompt "\nCommand: Wall Draw...")(terpri)
   (setq d1 (getdist st1))
   (if (= d1 nil)(setq d1 d2) )
   (setq e1 (entsel "\nPick line -> "))
   (setq d3 (/ d1 2))
   (while (/= e1 nil)
      (setq p1 (osnap (cadr e1) "END")
            p2 (osnap (cadr e1) "MID")
            ang1 (angle p1 p2)
            ang2 (+ ang1 (/ pi 2) )
            ang3 (- ang1 (/ pi 2) )
            p3 (polar p2 ang2 100)
            p4 (polar p2 ang3 100)
      )
      (command "offset" d3 e1 p3 "")
      (command "change" "l" "" "p" "la" cl "")
      (command "offset" d3 e1 p4 "")
      (command "change" "l" "" "p" "la" cl "")
       (setq e1 (entsel "\nPick line -> "))
   )
   (setvar "osmode" os)
   (setq wallthk (fix d1))(princ)
)





;******************** TEXT SIZE CHANGE = TS ******************************************
;;TEXT SIZE CHANGE (92cho_i)
(defun c:TS (/ ss n k en h h1 h2 hc)
   (prompt "\nCommand: Text Size Change..")
   (prompt "\nSelect Text ")
   (setq ss (ssget))
   (setq n (sslength ss))
   (setq k 0)
   (setq h2 (* (getvar "dimscale") 3))
   (setq h1 (cdr (assoc 40 (entget (ssname ss (1- n))))))
   (if (= h1 nil) (setq h1 h2))
   (setq hc (strcat "New Text Size<" (rtos h1 2 0) ">:"))
   (setq h (getreal hc))
   (if (= h nil) (setq h h1))
   (while (<= 1 n)
       (setq en (ssname ss k))
       (if (eq (cdr (assoc 0 (entget en))) "TEXT")
           (progn
              (entmod (subst (cons 40 h) (assoc 40 (entget en))(entget en)))
              (redraw en 4)
           )
       )
       (setq n (- n 1))
       (setq k (+ k 1))
    )
    (princ)
)




;******************** TEXT 1:1 SELECT CHANGE = TC ******************************************
(defun c:TC(/ ss1 en1 li1 tex1 ss1 li2 tex2)
  (setq ss1 (entsel "\nText Object1 "))
  (setq en1 (car ss1))
  (redraw en1 3)
  (setq li1 (assoc 1 (entget en1)))
  (setq tex1 (cdr li1)) (prin1 tex1) (terpri)
  (setq ss2 (entsel "\nText Object2 "))
  (setq li2 (assoc 1 (entget (car ss2))))
  (setq tex2 (cdr li2)) (prin1 tex2) (terpri)
  (entmod (subst (cons 1 tex2) (assoc 1 (entget en1))(entget en1)))
  (princ)
)




;******************** TEXT 1:1 등가교환(2개의 문자열) = TCC ***********************************
;;=========================================================
;  문자열 1:1 맞대응 교환하기(2011.7.22)  
;  -> 선택한 두개의 문자열을 서로 맞 교환 시켜주는 명령어
;  -> 선택한 문자열이 두개일 경우만 실행 됨
;  -> 아저씨님 개발하신 드림 툴의 문자 내용 서로 바꾸기(tsw)와 같은 명령어
;  -> 창문들 7번 64 bit에서도 실행 가능
;  -> 출처 : da........@nate.com
;;----------------------------------------------------------

(defun c:TCC (/ ss obj1 obj2 str1 str2) ; 명령어 본인이 원하는 단어로 바꾸어서 사용할수 도 있어요
        (prompt " .........문자열 1:1 맞교환하기...   A <-> B   선택문자가 2개인 경우만 1:1 맞교환됨 ")
  (if (and
	(setq ss (ssget '((0 . "TEXT")))) ;문자를 선택해라
	(= (sslength ss) 2) ;선택한 문자가 두개일경우만 실행시켜라
	(setq obj1 (vlax-ename->vla-object (ssname ss 0)))
	(setq obj2 (vlax-ename->vla-object (ssname ss 1)))
	(setq str1 (vla-get-textstring obj1))
	(setq str2 (vla-get-textstring obj2))
      )
      (progn
	(vla-put-textstring obj1 str2)
	(vla-put-textstring obj2 str1)
      )
  )
)
	




;******************** TEXT ALL 선택한 모든문자 변경 = TCA ***********************************
; 아키모아 운영진 "행복한하루"
; http://cafe.daum.net/archimore

(defun C:TCA(/ dec ent ed ena otex var ss k ed1)
 (princ " \n>> 문자 내용 동일화 (Text All Same...)")
 (princ "\n선택된 문자를 모두 동일한 문자 내용으로 변경합니다!")
 (setq dec (getvar "dimdec"))
 (if (setq ent (entsel "\nOriginal object select:"))
  (progn (redraw (car ent) 3)
   (setq ed (entget (car ent))) 
   (setq ena (cdr (assoc 0 ed)))
   (cond ((or (= ena "TEXT") (= ena "MTEXT")) (setq otex (assoc 1 ed)))
         ((= ena "DIMENSION") 
          (setq var (cdr (assoc 1 ed)))
          (if (= var "") 
           (setq otex (cons 1 (rtos (cdr (assoc 42 ed)) 2 dec)))
           (setq otex (assoc 1 ed))
          )
         )
   )
   (princ "\nAlteration object select:")
   (if (setq ss (ssget (list (cons 0 "text,mtext,dimension"))))
    (progn (setq k 0) 
     (repeat (sslength ss) 
      (setq ed1 (entget (ssname ss k)))
      (entmod (subst otex (assoc 1 ed1) ed1))
      (setq k (1+ k))
     ) ;repeat
    );progn
   );if
  (redraw (car ent) 4)
  );progn
 );if
(princ) 
)


;                                                      
; 선택한 객체의 문자,치수,블럭내문자,블럭내치수를 삭제 
;Text Sellect Delete

(defun delblock ( e / obj blname blocks blobj ee een eename item )
  (setq obj (vlax-ename->vla-object e))
  (setq blname (vla-get-name obj))
  (setq blocks (vla-get-blocks doc))
  (setq blobj (vla-item blocks blname))
  (vlax-for item blobj
    (setq ee (vlax-vla-object->ename item))
    (setq een (entget ee))
    (setq eename (cdr (assoc 0 een)))
    (if (or (= eename "TEXT") (= eename "MTEXT") (= eename "LEADER") (= eename "MULTILEADER") (= eename "DIMENSION")) (vla-delete item))
  )
)

(defun c:tsd ( / ss doc index e en ename )
  (if (setq ss (ssget (list (cons 0 "TEXT,MTEXT,DIMENSION,LEADER,MULTILEADER,INSERT"))))
    (progn
      (setq doc (vla-get-activedocument (vlax-get-acad-object)))
      (vla-startundomark doc)
      (setq index 0)
      (repeat (sslength ss)
	(setq e (ssname ss index))
	(setq en (entget e))
	(setq ename (cdr (assoc 0 en)))
	(cond
	  ((or (= ename "TEXT") (= ename "MTEXT") (= ename "LEADER") (= ename "MULTILEADER") (= ename "DIMENSION")) (entdel e))
	  ((= ename "INSERT") (delblock e))
	)
	(setq index (1+ index))
      )
      (vla-regen doc acAllViewports)
      (vla-endundomark doc)
    )
  )
  (princ)
)
(vl-load-com)
;(prompt "\n[ EB ]")
(princ)


*************************************** TAV *************************************************
;********************************
; Program : TAV
;           Text Arrange Vertical
;           By Suk-Jong Yi
;           1995/5/27
;********************************

(defun C:TAV(/
ents nent ans w72 xpnt count ent
old10 old11 old72 new10 new11 new72

)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

(setq ents (ssget))
(setq nent (sslength ents))

(initget "Left Middle Right")                    ;
(setq ans (getkword "\n<Left>/Middle/Right: "))
(if (= ans nil) (setq ans "Left"))
(cond
  ((= ans "Left")   (setq w72 0))
  ((= ans "Middle") (setq w72 1))
  ((= ans "Right")  (setq w72 2))
) ;of cond

(setq xpnt (getpoint "\nPick Point: "))

(setq count 0)
(repeat nent
  (setq ent (entget (ssname ents count)))
  (if (= "TEXT" (cdr (assoc 0 ent)))
    (progn
      (setq old10 (assoc 10 ent))                 
      (setq old11 (assoc 11 ent))                 
      (setq old72 (assoc 72 ent))                 
      (setq new10 (cons 10 (list (car xpnt) (cadr (cdr old10)))))
      (setq new11 (cons 11 (list (car xpnt) (cadr (cdr old11)))))
      (setq new72 (cons 72 w72))
      (if (= (cdr old72) 0)
        (progn                                    ; left
          (if (= w72 0)
              (setq ent (subst new10 old10 ent))
            (progn
               (setq new11 (cons 11 (list (car xpnt) (cadr (cdr old10)))))
               (setq ent (subst new11 old11 ent))
            );of progn
          ) ;of if
          (setq ent (subst new72 old72 ent))
        ) ;of progn
        (progn                                           ; left
          (if (= w72 0)
             (setq ent (subst new10 old10 ent))
             (setq ent (subst new11 old11 ent))
          ) ;of if
          (setq ent (subst new72 old72 ent))
        ) ;of progn
      ) ;of if
      (entmod ent)
    ) ;of progn
  ) ;of if
  (setq count (+ count 1))
);of repeat
  (setq *error* oer seterr nil)
(princ)
) ;of defun


;********************* CHANGE M2 2 PY = PY *** ***********************************************
(defun c:PY(/ choi d_ts w_os ht ss1 en1 ent1 tv1 grpt pt1 rst1
                      tot1 rtv1 rtv2 en an)
   (prompt "... 평구하기")
   (setq choi (getvar "orthomode") w_os (getvar "osmode") )
   (setvar "orthomode" 0) (setvar "osmode" 0)
   (prompt "\nSelect number->")
   (setq ss1 (entsel) en1 (car ss1)) (redraw en1 3)
   (setq ent1 (assoc 1 (entget (car ss1))))
   (setq ht  (assoc 40 (entget (car ss1))))
   (setq tv1 (cdr ent1) ht (cdr ht))
   (setq grpt (grread T) pt1 (cadr grpt))
   (setq  rst1 0)
   (setq rtv1 (atof tv1))
;   (setq rst1 (/ rtv1 3.3058))
   (setq rst1 (* rtv1 0.3025))
   (setq tot1 (abs rst1))
   (setq tot1 (rtos tot1 2 2))
   (if (= (wcmatch tot1 "*.*") nil)
       (setq tot1 (strcat tot1 ".00")))
   (setq an (strlen tot1))
   (if (= (substr tot1 (- an 1) 1) ".")
       (setq tot1 (strcat tot1 "0")) )
   (if (< rst1 0)
       (setq tot1 (strcat "-" tot1)))
   (command "text" "j" "r" pt1 ht "0" tot1)
   (redraw en1 4)
   (prompt "\nEnter text point->")
   (command "move" "l" "" pt1 pause)
;   (prompt "\n결 과 : ") (prin1 tv1) (prompt " / 3.3058 = ")(prin1 rst1)
   (prompt "\n결 과 : ") (prin1 tv1) (prompt " * 0.3025 = ")(prin1 rst1)
   (setvar "orthomode" choi) (setvar "osmode" w_os)
   (prin1)
)



;************************* QUICK AREA = QA ***************************************************
(defun c:QA(/  myerror choi d_ts w_os grpt pt1 ent1 en1 ar1 ar2 ar3 ar4 ar5 an arm ht)
;start --- Internal error handler ----------
 (defun myerror(S)
 (if (/= s "Function cancelled")(princ (strcat "\nError:" s)))
 (setvar "osmode" w_os)
 (setq *error* olderr)(princ) )
 (setq olderr *error* *error* myerror)
;end----------------------------------------
   (prompt "... 면적구하기")
   (setq choi (getvar "orthomode")  d_ts (getvar "textsize")
            w_os (getvar "osmode") )
   (setvar "orthomode" 0) (setvar "osmode" 0)
   (setq ht d_ts)
   (prompt "\nText Height<") (prin1 ht) (prompt ">: ")
   (setq ht (getdist))
   (if (= ht nil) (setq ht d_ts))
;;
   (setq ent1 (entsel  "\nPline 선택->"))
   (if (/= ent1 nil)(progn
       (setq en1 (car ent1)) (redraw en1 3)
       (setq grpt (grread T))
       (setq pt1 (cadr grpt))
       (command "area" "e" ent1)
   ))
;-boundary start
   (if (= ent1 nil)(progn
       (prompt "\nSelect internal point->")
       (command "boundary" pause "")
       (setq en1 (entlast))(redraw en1 3)
       (command "area" "e" "l")
       (setq pt1 (getvar "lastpoint"))
   ));-boundary end
   (setq ar1 (/ (getvar "area") 1.0e+02))
   (setq ar2 (rtos ar1 2 0))
   (setq an (strlen ar2))
   (setq arm (substr ar2 (- an 1) 1))
   (setq arm (atof arm))
   (setq ar3 (atoi ar2))
   (if (>= arm 5)(progn
       (setq ar3 (+ ar3 100))
   ))
   (setq ar3 (rtos ar3 2 0))
   (setq ar4 (strcat (substr ar3 1 (- an 4)) "." (substr ar3 (- an 3) 2)   ))
   (if (= (substr ar4 1 1) ".") (setq ar4 (strcat "0" ar4)))
   (setq ar5 (strcat (substr ar2 1 (- an 4)) "." (substr ar2 (- an 3) 3)   ))
   (if (= (substr ar5 1 1) ".") (setq ar5 (strcat "0" ar5)))
   (command "text" "j" "r" pt1 ht "0" ar4)
   (prompt "\nEnter text point->")
   (command "move" "l" "" pt1 pause)
   (redraw en1 4)
   (setvar "orthomode" choi) (setvar "osmode" w_os)
   (prompt "\nArea : ")(prin1 ar5) (prompt "  or  ")(prin1 ar4)
   (prin1)
)




;************************* Number Plus-숫자더하기 = NP *****************************************
;  Number plus(98CHO_I)
;  ->숫자의 합 구하기(2자리)
;;-----------------------------------------------------------
(defun c:np(/  myerror choi d_ts w_os ht ss grpt pt1 n k tot en tv rtv an)
;start --- Internal error handler ----------
 (defun myerror(S)
 (if (/= s "Function cancelled")(princ (strcat "\nError:" s)))
 (setvar "osmode" w_os)
 (setq *error* olderr)(princ) )
 (setq olderr *error* *error* myerror)
;end----------------------------------------
   (setq choi (getvar "orthomode")  d_ts (getvar "textsize")
            w_os (getvar "osmode") )
   (prompt "... 더하기")
   (setvar "orthomode" 0) (setvar "osmode" 0)
   (setq ht d_ts)
   (prompt "\nText Height<") (prin1 ht) (prompt ">: ")
   (setq ht (getdist))
   (if (= ht nil) (setq ht d_ts))
   (prompt "\nSelect number->")
   (setq ss (ssget))
   (setq grpt (grread T))
   (setq pt1 (cadr grpt))
   (setq n (sslength ss))
   (setq k 0 tot 0)
   (while (<= 1 n)
        (setq en (ssname ss k))
        (setq tv (cdr (assoc 1 (entget en))))
        (setq rtv (atof tv))
        (setq tot (+ tot rtv))
        (setq n (- n 1))
        (setq k (+ k 1))
   )
   (setq tot (rtos (* tot 10000) 2 0))
   (setq an (strlen tot))
   (setq tot (strcat (substr tot 1 (- an 4)) "." (substr tot (- an 3) 2)   ))
   (command "text" "j" "r" pt1 ht "0" tot)
   (prompt "\nEnter text point->")
   (command "move" "l" "" pt1 pause)
   (setvar "orthomode" choi) (setvar "osmode" w_os)
   (prompt "\n합 계 : ") (prin1 tot)
   (prin1)
)




;************************* Number Minus-숫자빼기 = NM *************************************************
;  Number minus(98CHO_I)
;  ->두 수의 차 구하기(2자리)
;;-----------------------------------------------------------
(defun c:nm(/   myerror choi d_ts w_os ht ss1 en1 ent1 tv1 ss2 en2 ent2 tv2 grpt pt1 rst1
                      tot1 rtv1 rtv2 en an)
;start --- Internal error handler ----------
 (defun myerror(S)
 (if (/= s "Function cancelled")(princ (strcat "\nError:" s)))
 (setvar "osmode" w_os)
 (setq *error* olderr)(princ) )
 (setq olderr *error* *error* myerror)
;end----------------------------------------
   (prompt "... 빼기")
   (setq choi (getvar "orthomode")  d_ts (getvar "textsize")
            w_os (getvar "osmode") )
   (setvar "orthomode" 0) (setvar "osmode" 0)
   (setq ht d_ts)
   (prompt "\nText Height<") (prin1 ht) (prompt ">: ")
   (setq ht (getdist))
   (if (= ht nil) (setq ht d_ts))
   (prompt "\nSelect first number->")
   (setq ss1 (entsel) en1 (car ss1))   (redraw en1 3)
   (setq ent1 (assoc 1 (entget (car ss1))))
   (setq tv1 (cdr ent1))
   (prompt "\nSelect second number->")
   (setq ss2 (entsel) en2 (car ss2))   (redraw en2 3)
   (setq ent2 (assoc 1 (entget (car ss2))))
   (setq tv2 (cdr ent2))
   (setq grpt (grread T) pt1 (cadr grpt))
   (setq  rst1 0)
   (setq rtv1 (atof tv1)  rtv2 (atof tv2))
   (setq rst1 (- rtv1 rtv2))
   (setq tot1 (abs rst1))
   (setq tot1 (rtos tot1 2 2))
   (if (= (wcmatch tot1 "*.*") nil)
       (setq tot1 (strcat tot1 ".00")))
   (setq an (strlen tot1))
   (if (= (substr tot1 (- an 1) 1) ".")
       (setq tot1 (strcat tot1 "0")) )
   (if (< rst1 0)
       (setq tot1 (strcat "-" tot1)))
   (command "text" "j" "r" pt1 ht "0" tot1)
   (redraw en1 4) (redraw en2 4)
   (prompt "\nEnter text point->")
   (command "move" "l" "" pt1 pause)
   (setvar "orthomode" choi) (setvar "osmode" w_os)
   (prompt "\n결 과 : ")(prin1 rtv1)(prompt "-")
   (prin1 rtv2)(prompt "= ")(prin1 rst1)
   (prin1)
)




;************************* Number By-숫자곱하기 = NB **************************************************
; Number by(0010cho_i)
; ->두 수의 곱 구하기(2자리)
;;-----------------------------------------------------------
(defun c:nb(/  myerror choi d_ts w_os ht ss1 en1 ent1 tv1 ss2 en2 ent2
              tv2 grpt pt1 rst1 tot1 rtv1 rtv2 en an)
;start --- Internal error handler ----------
 (defun myerror(S)
 (if (/= s "Function cancelled")(princ (strcat "\nError:" s)))
 (setvar "osmode" w_os)
 (setq *error* olderr)(princ) )
 (setq olderr *error* *error* myerror)
;end----------------------------------------
   (prompt "... 곱하기")
   (setq choi (getvar "orthomode")  d_ts (getvar "textsize")
            w_os (getvar "osmode") )
   (setvar "orthomode" 0) (setvar "osmode" 0)
   (setq ht d_ts)
   (prompt "\nText Height<") (prin1 ht) (prompt ">: ")
   (setq ht (getdist))
   (if (= ht nil) (setq ht d_ts))
   (prompt "\nSelect first number->")
   (setq ss1 (entsel) en1 (car ss1))   (redraw en1 3)
   (setq ent1 (assoc 1 (entget (car ss1))))
   (setq tv1 (cdr ent1))
   (prompt "\nSelect second number->")
   (setq ss2 (entsel) en2 (car ss2))   (redraw en2 3)
   (setq ent2 (assoc 1 (entget (car ss2))))
   (setq tv2 (cdr ent2))
   (setq grpt (grread T) pt1 (cadr grpt))
   (setq  rst1 0)
   (setq rtv1 (atof tv1)  rtv2 (atof tv2))
   (setq rst1 (* rtv1 rtv2));곱하기
   (setq tot1 (abs rst1))
   (setq tot1 (rtos tot1 2 2))
   (if (= (wcmatch tot1 "*.*") nil)
       (setq tot1 (strcat tot1 ".00")))
   (setq an (strlen tot1))
   (if (= (substr tot1 (- an 1) 1) ".")
       (setq tot1 (strcat tot1 "0")) )
   (if (< rst1 0)
       (setq tot1 (strcat "-" tot1)))
   (command "text" "j" "r" pt1 ht "0" tot1)
   (redraw en1 4) (redraw en2 4)
   (prompt "\nEnter text point->")
   (command "move" "l" "" pt1 pause)
   (setvar "orthomode" choi) (setvar "osmode" w_os)
   (prompt "\n결 과 : ")(prin1 rtv1)(prompt "*")
   (prin1 rtv2)(prompt "= ")(prin1 rst1)
   (prin1)
)




;************************* Number Divide-숫자나누기 = ND **********************************************
; Number divide('0106cho_i)
; ->두 수의 나누기 구하기(2자리)
;   작은수을 큰수로 나누면 percent(--%)로 표시됨
;;---------------------------------------------------------
(defun c:nd(/  myerror choi d_ts w_os ht ss1 en1 ent1 tv1 ss2 en2 ent2
             tv2 grpt pt1 rst1 tot1 rtv1 rtv2 en an)
;start --- Internal error handler ----------
 (defun myerror(S)
 (if (/= s "Function cancelled")(princ (strcat "\nError:" s)))
 (setvar "osmode" w_os)
 (setq *error* olderr)(princ) )
 (setq olderr *error* *error* myerror)
;end----------------------------------------
   (prompt "... 나누기")
   (setq choi (getvar "orthomode")  d_ts (getvar "textsize")
            w_os (getvar "osmode") )
   (setvar "orthomode" 0) (setvar "osmode" 0)
   (setq ht d_ts)
   (prompt "\nText Height<") (prin1 ht) (prompt ">: ")
   (setq ht (getdist))
   (if (= ht nil) (setq ht d_ts))
   (prompt "\nSelect first number->")
   (setq ss1 (entsel) en1 (car ss1))   (redraw en1 3)
   (setq ent1 (assoc 1 (entget (car ss1))))
   (setq tv1 (cdr ent1))
   (prompt "\nSelect second number->")
   (setq ss2 (entsel) en2 (car ss2))   (redraw en2 3)
   (setq ent2 (assoc 1 (entget (car ss2))))
   (setq tv2 (cdr ent2))
   (setq grpt (grread T) pt1 (cadr grpt))
   (setq  rst1 0)
   (setq rtv1 (atof tv1)  rtv2 (atof tv2))
   (setq rst1 (/ rtv1 rtv2));나누기
   (if (< rtv1 rtv2) (setq rst1 (* rst1 100)))
   (setq tot1 (abs rst1))
   (setq tot1 (rtos tot1 2 2))
   (if (= (wcmatch tot1 "*.*") nil)
       (setq tot1 (strcat tot1 ".00")))
   (setq an (strlen tot1))
   (if (= (substr tot1 (- an 1) 1) ".")
       (setq tot1 (strcat tot1 "0")) )
   (if (< rst1 0)
       (setq tot1 (strcat "-" tot1)))
   (command "text" "j" "r" pt1 ht "0" tot1)
   (redraw en1 4) (redraw en2 4)
   (prompt "\nEnter text point->")
   (command "move" "l" "" pt1 pause)
   (setvar "orthomode" choi) (setvar "osmode" w_os)
   (prompt "\n결 과 : ")(prin1 rtv1)(prompt "/")
   (prin1 rtv2)(prompt "= ") (prin1 rst1)
   (if (< rtv1 rtv2)(progn
       (prompt "\n결 과 : ")(prin1 rtv1)(prompt "/")(prin1 rtv2)
       (prompt "*100")(prompt "= ") (prin1 rst1)(prompt"%")
   ))
   (prin1)
)



********************* Numbering = Nud **********************************************

(defun c:nub ()
  (setq s_txt (getreal "\nText height "))
  (setq s_num (getstring "\nbeginning number : "))
  (if (= s_num "") (setq s_num num) (setq num (itoa (1- (read s_num)))))
  (while (setq txt_pt (getpoint "\ntext insertion point: "))
  (command "._text" txt_pt s_txt "" (if (not num) (setq num "0") (setq num (itoa (1+ (read num)))))""))
  (princ)
  )



********************* Numbering ABC = Nua **********************************************

(defun c:nua ( / ss i num ent)
 (if (setq ss (ssget '((0 . "text"))))
  (progn
   (setq i -1 num 1)
   (repeat (sslength ss)
    (setq ent (entget (ssname ss (setq i (1+ i)))))
    (entmod (subst (cons 1 (chr (+ 64 num))) (assoc 1 ent) ent))
    (setq num (if (>= num 26) 1 (1+ num)))
   )
  )
 )
 (princ)
)



;************************* LINE JOIN = LJ **************************************************************
;;================================================
;  Line Join (93cho_i)
;;------------------------------------------------
(defun c:LJ(/ ot1 os e1 e2 p1 p2 p3 d1 ang1)
   (setq ot1 (getvar "orthomode")
         os (getvar "osmode"))
   (setvar "orthomode" 0)(setvar "osmode" 0)
   (prompt "\nCommand: Line Joint...")
   (setq e1 (entsel "\nPick first line-->"))
   (setq e2 (entsel "\nPick second line-->"))
   (setq p1 (osnap (cadr e2) "END")
         p2 (osnap (cadr e2) "MID")
         ang1 (angle p1 p2)
         d1 (distance p1 p2)
         p3 (polar p2 ang1 d1)
   )
   (setq e2 (car e2))
   (entdel e2)
   (command "change" e1 "" p3)
   (setvar "orthomode" ot1)
   (setvar "osmode" os)
   (prompt "\nCommand:")
   (prin1)
)






;************************* Line PLUS - 라인길이더하기 = LP **********************************************
;;선택된 Line의 길이을 모두 더하는 명령('010607cho_i)
(defun c:LP(/  ss en n n1 k tot dis dis1 t6 tn t3 ht)
   (prompt "\nCommand: Line Plus...")
   (prompt "\n더하고자하는 line을 선택-> ")
   (setq ss (ssget))
   (setq n1 (sslength ss))
   (setq n n1 k 0)
   (setq tot 0 dis 0 dis1 0)
   (while (<= 1 n)
       (setq en (ssname ss k))
       (setq stype (cdr (assoc 0 (entget en))))
       (if (= stype "LINE")(progn
           (setq spt (cdr (assoc 10 (entget en))))
           (setq ept (cdr (assoc 11 (entget en))))
           (setq dis1 (distance spt ept))
           (setq dis (/ dis1 1000.0))(prin1 dis)
       ))(terpri)
       (if (= stype "LWPOLYLINE")(progn
           (command "area" "e" en)
           (setq dis1 (getvar "perimeter"))
           (setq dis (/ dis1 1000.0))
       ))
       (setq tot (+ tot dis))
       (setq n (- n 1))
       (setq k (+ k 1))
    )(terpri)
;;추가 시작
    (setq t6 (rtos tot 2 3))
    (setq tn (strlen t6))
    (setq t3 (substr t6 1 tn))
    (setq tot t3)
    (setq pt1 (getvar "lastpoint"))
    (setq ht (getvar "textsize"))
    (command "text" "j" "r" pt1 ht "0" tot)
    (prompt "\nEnter text point->")
    (command "move" "l" "" pt1 pause)
;;추가 끝
    (prompt "총")(prin1 n1)(prompt "개의 라인...")
    (prompt "전체길이 : ")(prin1 tot)(prompt " m")
    (prin1)
)






;********************* 3d2d-모든객체 동일평면값으로 = 3D2D **********************************|;

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


(defun 3d2d_main(  / 
                      dcl_id3 selobj selnum idnum ed obj name starttime endtime
                    )
  
  (setq oldcmd (getvar "cmdecho"))
  (setvar "cmdecho" 0)      

  (princ "\n>>Select Object<<")
  (setq selobj (ssget))  
  (setq selnum (sslength selobj))  
  (setq starttime (getvar "DATE"))
  (setq starttime (fix (* (- starttime (fix starttime)) 100000.)))  
  (setq idnum 0.)
  (repeat selnum
    (setq ename (ssname selobj idnum))
    (setq ed (entget ename))
    (if (= (cdr (assoc 0 ed)) "POLYLINE")
      (progn ;THEN
        (setq nxt ename)                                ;첫 엔티티 이름
        (while (setq nxt (entnext nxt))
            (setq ent (entget nxt))                        ;엔티티정보축출
            (if (= (cdr (assoc 0 ent)) "VERTEX")           ;절점일때만
              (progn
		(setq oldpnt (cdr (assoc 10 ent)))
	        (setq ent (subst (cons 10 (list (car oldpnt) (cadr oldpnt) 0))
				 (assoc 10 ent) ent))
	        (entmod ent)
	        (entupd nxt)
	      );progn
            );if
        );while
      );progn
      (progn ;ELSE
        (setq o38 (assoc 38 ed))
        (setq n38 (cons (car o38) 0.0))
        (setq ed (subst n38 o38 ed))
        (setq obj (cdr (assoc 0 ed)))
        (setq name (cdr (assoc 1 ed)))
        (setq ed (noele ed 10))
        (setq ed (noele ed 11))
        (setq ed (noele ed 13))
        (setq ed (noele ed 14))    
        (setq ed (noele ed 21)) 
        (setq ed (noele ed 30)) 
        (setq ed (noele ed 31)) 
        (entmod ed) 
      );progn	
    );if  
    (setq idnum (1+ idnum))
    (princ (strcat "Converting Data Num : " (rtos idnum 2 0) "(" (rtos (* (/ idnum selnum) 100) 2 0) "%)" (chr 13)))
  );repeat
  (setvar "ELEVATION" 0)
  (setq endtime (getvar "DATE"))
  (setq endtime (fix (* (- endtime (fix endtime)) 100000.)))  
  (princ (strcat "\nElapsed Time : " (chtime (- endtime starttime)) "\nConverting End....."))
  (setq *error* OLDERROR)
  (setvar "cmdecho" OLDCMD)
  (princ)           
);defun
;|**********************************************************************|;

(defun C:3d2d() (3d2d_main))
(princ "\nLoading Complete......  Programed by Shin Jong-Hwa by DooSan ENG.\nType : 3d2d")
(princ)




;**************************** 문자앞에 파이넣기 = PI ****************************
;;============================================================
;   선택한 치수 or 문자에 파이(Ø) 넣기 (2007.3. 주말농부)
;   ->개선내용 1) 치수 와 문자에 모두적용
;                    2) 앞에 파이(Ø)가 있으면 추가하지않음
;;----- pi add <파이 에드>--------------------------------------
(defun c:pi (/ os ss sslen ass1 otxt ntxt elist k)
   (setvar "cmdecho" 0)
   (setq os (getvar "osmode"))
   (setvar "osmode" 0)
   (prompt "\n파이(Ø) 넣기할 치수나 문자선택-> ")
   (setq ss (ssget))
   (setq sslen (sslength ss) k 0)
   (while (< k sslen)
       (setq elist (entget (ssname ss k)) )
       (setq ass1 (assoc 1 elist) otxt (cdr ass1) )
       (if (= otxt "") (setq otxt "<>"))
       (if (and (= (wcmatch otxt "%%216*") nil) (= (wcmatch otxt "%%216*") nil))
           (progn
              (setq ntxt  (strcat "%%216" otxt ) elist (subst (cons 1 ntxt) ass1 elist))
              (entmod elist) 
           )
       )
       (setq k (+ K 1))
   )
   (setvar "osmode" os)
   (prin1)	
)




;*********************** 스케일리스트에디트-재설정 = SE *********************************
(defun c:SE()(command "-SCALELISTEDIT" "R" "Y" "E")
             (princ "\n Command=-ScaleListEdit_r")(prin1))



;*********************** 도면요소 퍼지 = -P *********************************************
(defun c:-P()(command "-PURGE" "A" "*" "N")
             (princ "\n Command=-PURGE")(prin1))





;*********************** 콤마인아웃 = CM *********************************************
;;==================================
;  Comma i & o ('00.11-'01.11cho_i)
; -> 천단위의 컴마 찍기 및 없애기
;;----------------------------------
(defun c:cmo(/ ss n k kn kk en kna0 a1 a2 a3)
   (prompt "\n컴마 없애기-> ")
   (setq ss (ssget))
   (setq n (sslength ss))
   (setq k 0)
   (while (<= 1 n)
       (setq en (ssname ss k))
       (if (eq (cdr (assoc 0 (entget en))) "TEXT")
           (progn
           (setq ent1 (assoc 1 (entget en)))
           (setq txt1 (cdr ent1))
           (setq kn (strlen txt1))
           (if (/= (wcmatch txt1 "*,*") nil)(progn
               (setq kk 2)
               (setq a0 (substr txt1 KK 1))
               (while (and (/= a0 ",")(<= kk kn))
                   (setq kk (+ kk 1))
                   (setq a0 (substr txt1 KK 1))
               )
               (if (= a0 ",")(progn
                   (setq a1 (substr txt1 1 (- kk 1)))
                   (setq a2 (substr txt1 (+ kk 1) (- kn kk)))
                   (setq a3 (strcat a1 a2))
                   (entmod (subst (cons 1 a3) (assoc 1 (entget en))(entget en)))
               ))
           ))
       ))
       (setq n (- n 1))
       (setq k (+ k 1))
    )
   (prin1)
)
(defun c:cmi(/ ss n k en ent1 txt1 kn txt2 a1 a2 a3)
   (prompt "\n컴마 찍기 -> ")
   (setq ss (ssget))
   (setq n (sslength ss))
   (setq k 0)
   (while (<= 1 n)
       (setq en (ssname ss k))
       (if (eq (cdr (assoc 0 (entget en))) "TEXT")
           (progn
           (setq ent1 (assoc 1 (entget en)))
           (setq txt1 (cdr ent1))
           (setq kn (strlen txt1))
           (setq txt2 (atof txt1))
           (cmo_1 txt2)
           );progn end
       );if end
       (setq n (- n 1))
       (setq k (+ k 1))
    );while end
   (prin1)
)
;cmo sub routine
(defun cmo_1(txt2)
   (if (and (>= txt2 1000)(< txt2 10000))(progn
       (setq a1 (substr txt1 1 1))
       (setq a2 (substr txt1 2 (- kn 1)))
       (setq a3 (strcat a1 "," a2))
       (entmod (subst (cons 1 a3) (assoc 1 (entget en))(entget en)))
   ))
   (if (and (>= txt2 10000)(< txt2 100000))(progn
       (setq a1 (substr txt1 1 2))
       (setq a2 (substr txt1 3 (- kn 2)))
       (setq a3 (strcat a1 "," a2))
       (entmod (subst (cons 1 a3) (assoc 1 (entget en))(entget en)))
   ))
   (if (and (>= txt2 100000)(< txt2 1000000))(progn
       (setq a1 (substr txt1 1 3))
       (setq a2 (substr txt1 4 (- kn 3)))
       (setq a3 (strcat a1 "," a2))
       (entmod (subst (cons 1 a3) (assoc 1 (entget en))(entget en)))
   ))
)




;*********************** 블럭이름 바꾸기 = BN *********************************************

;; Title : Block Name Change
;; Shoutcut key : bn
;; Description : 블럭을 선택하여 블럭이름을 바꾸는 명령어
;; Maker : 원작(2006.10. 주말농부)
;;         rev 1. 2007.03 / DCL을 이용한 이름 변경(by 주말농부)
;;         rev 2. 2009.05 / DCL을 LSP단일 파일로 흡수 & Code 정리 (by nadau)


(defun c:bn(/ os ent elist ins) 
    (setvar "cmdecho" 0)
    (setq os (getvar "osmode"))
    (if (setq ent (car (entsel "\nBlock select:")))
      (progn (setq elist (entget ent))
        (setq obn (cdr (assoc 2 elist)))
        (setq ins (cdr (assoc 10 elist)))
        (setq rname nil)

        (while (= rname nil)
          (setq rname (getstring t "\nNew block name:"))
          (setq byn (assoc 2 (tblsearch "block" rname)))
          (setq byn2 (cdr byn)) ; byn2 블럭이름
          (if (= rname byn2) 
            (progn (setq rname nil)
               (prompt "블럭이름중복->다시입력해주세요")
            ) ;progn
          );if 
        ) ;while

        (if (/= rname "") 
          (progn (command "explode" ent)
            (setvar "osmode" 0)
            (command "_.block" rname ins "P" "")
            (command "_.insert" rname ins "" "" "")
            (setvar "osmode" os)
          (princ "블록이름: ")(princ obn)(princ " --> ")(princ rname)(princ " 로 교체함") 
          );progn
        );if 
      );progn
     );if 
(princ)
);defun







;*********************** 블럭인서트 변경 = CBI  *********************************************

;;---------------=={ Change Block Insertion }==---------------;;
;;                                                            ;;
;;  Modifies the Block Definition Base Point of a selected    ;;
;;  block to a user specified point.                          ;;
;;------------------------------------------------------------;;
;;  Author: Lee McDonnell                                     ;;
;;                                                            ;;
;;  Copyright 2010 by Lee McDonnell, All Rights Reserved.   ;;
;;  Contact: Lee Mac @ TheSwamp.org, CADTutor.net             ;;
;;                                                            ;;
;;  With Thanks to Gilles Chanteau for his excellent advice.  ;;
;;------------------------------------------------------------;;
;;  Version:  1.1   ~¤~   11 June 2010                        ;;
;;------------------------------------------------------------;;

;; -- Retains Insertion Point --
;;(defun c:CBI  nil (ChangeBlockInsertion nil))

;; -- Retains Block Position --
(defun c:CBI nil (ChangeBlockInsertion   t))


;;------------------{ Local Functions }------------------------;;

(defun ChangeBlockInsertion ( retain / *error* doc blocks oldc
                                       SourceBlock pt undo vec name ss )
  ;; Lee Mac  ~  11.06.10
  (vl-load-com)

  (setq doc    (vla-get-ActiveDocument (vlax-get-acad-object))
        blocks (vla-get-blocks doc))

  (defun *error* ( msg )
    (and undo (vla-EndUndoMark doc))
    (and oldc (setvar 'CMDECHO oldc))
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )

  (setq oldc (getvar 'CMDECHO))
  (setvar 'CMDECHO 0)

  (if
    (and
      (setq SourceBlock
        (lm:SelectifFoo
          (lambda ( x )
            (if (eq "AcDbBlockReference"
                  (vla-get-ObjectName
                    (setq x (vlax-ename->vla-object x))
                  )
                )
              x
            )
          )
          "\nSelect Block: "
        )
      )
      (setq pt (getpoint "\nSpecify New Base Point: "))
    )
    (progn
      (setq undo (not (vla-StartUndoMark doc)))

      (setq Vec  (lm:ChangeBlockInsertion SourceBlock pt)
            Name (lm:GetBlockName SourceBlock))

      (if retain
        (
          (lambda ( i / ss e obj )
            (setq ss (ssget "_X" (list (cons 0 "INSERT") (cons 2 Name))))
            (while (setq e (ssname ss (setq i (1+ i))))
              (vla-Move (setq obj (vlax-ename->vla-object e))
                (vlax-3D-point '(0. 0. 0.))
                (vlax-3D-point (mxv (lm:Def->Geom obj) Vec))
              )
            )
          )
          -1
        )
      )

      (if (eq :vlax-true (vla-get-HasAttributes SourceBlock))
        (vl-cmdf "_.attsync" "_N" Name)
      )
      (vla-regen doc acAllViewports)
      (setq undo (vla-EndUndoMark doc))
    )
  )
  (setvar 'CMDECHO oldc)
  (princ)
)

;; ChangeBlockInsertion (Lee Mac)
;; Changes the block definiton insertion point calculated from
;; a reference block and user specified point.
;;
;; Arguments:
;; SourceBlock  ~  VLA Block Reference Object
;; Pt           ~  New Insertion Point relative to SourcBlock (UCS).

(defun lm:ChangeBlockInsertion ( SourceBlock pt / BlockDef Mat Vector )
  ;; Lee Mac  ~  11.06.10
  (vl-load-com)

  (if (and (setq BlockDef (lm:Itemp blocks (lm:GetBlockName SourceBlock)))
           (setq Mat (lm:Geom->Def SourceBlock)))
    (progn
      (setq Vector
        (mxv Mat
          (mapcar '- (setq pt (trans pt 1 0)) ; UCS->WCS
            (setq Insertion
              (vlax-get SourceBlock 'InsertionPoint)
            )
          )
        )
      )
      (vlax-for obj BlockDef
        (vla-Move obj (vlax-3D-point Vector) (vlax-3D-point '(0. 0. 0.)))
      )
    )
  )
  Vector
)

;; Geom->Def (Lee Mac)
;; Returns the Transformation Matrix for transforming Block
;; Geometry to the Block Definiton.
;;
;; Arguments:
;; SourceBlock  ~  VLA Block Reference Object
;; Returns: A 3x3 Transformation Matrix

(defun lm:Geom->Def ( SourceBlock / norm ang x y z )
  ;; Lee Mac  ~  11.06.10
  (vl-load-com)

  (setq norm (vlax-get SourceBlock 'Normal)
         ang (- (vla-get-rotation SourceBlock)))
      
  (mapcar 'set '(x y z)
    (mapcar
      '(lambda ( prop alt )
         (/ 1.
            (vlax-get-property SourceBlock
              (if (vlax-property-available-p SourceBlock prop) prop alt)
            )
          )
       )
      '(XEffectiveScaleFactor YEffectiveScaleFactor ZEffectiveScaleFactor)
      '(XScaleFactor          YScaleFactor          ZScaleFactor         )
    )
  )
  (mxm
    (list
      (list x 0. 0.)
      (list 0. y 0.)
      (list 0. 0. z)
    )
    (mxm
      (list
        (list (cos ang) (sin (- ang)) 0.)
        (list (sin ang) (cos ang)     0.)
        (list     0.        0.        1.)
      )
      (mapcar '(lambda ( e ) (trans e norm 0 t)) ; OCS->WCS
        '((1. 0. 0.) (0. 1. 0.) (0. 0. 1.))
      )
    )
  ) 
)

;; Def->Geom (Lee Mac)
;; Returns the Transformation Matrix for transforming
;; Block Definition Geometry to a Block Reference.
;;
;; Arguments:
;; SourceBlock  ~  VLA Block Reference Object
;; Returns: A 3x3 Transformation Matrix

(defun lm:Def->Geom ( SourceBlock / norm ang x y z )
  ;; Lee Mac  ~  11.06.10
  (vl-load-com)

  (setq norm (vlax-get SourceBlock 'Normal)
         ang (vla-get-rotation SourceBlock))
      
  (mapcar 'set '(x y z)
    (mapcar
      '(lambda ( prop alt )
         (vlax-get-property SourceBlock
           (if (vlax-property-available-p SourceBlock prop) prop alt)
         )
       )
      '(XEffectiveScaleFactor YEffectiveScaleFactor ZEffectiveScaleFactor)
      '(XScaleFactor          YScaleFactor          ZScaleFactor         )
    )
  )
  (mxm
    (mapcar '(lambda ( e ) (trans e 0 norm t)) ; WCS->OCS
      '((1. 0. 0.) (0. 1. 0.) (0. 0. 1.))
    )
    (mxm
      (list
        (list (cos ang) (sin (- ang)) 0.)
        (list (sin ang) (cos ang)     0.)
        (list     0.        0.        1.)
      )
      (list
        (list x 0. 0.)
        (list 0. y 0.)
        (list 0. 0. z)
      )
    )
  )
)

;; GetBlockName (Lee Mac)
;; Returns the block name as per the block definition
;;
;; Arguments:
;; obj  ~  VLA Block Reference Object
;; Returns: The Block Name as per the Block definition
  
(defun lm:GetBlockName ( obj )
  ;; Lee Mac  ~  11.06.10
  (vlax-get-property obj
    (if (vlax-property-available-p obj 'EffectiveName)
      'EffectiveName 'Name
    )
  )
)

;; Itemp (Lee Mac)
;; Returns the VLA-Object with index 'item' if
;; present in the collection, else nil.
;;
;; Arguments:
;; coll  ~  the VLA Collection Object
;; item  ~  the index to be retrieved.
;; Returns: the VLA-Object at the specified index
;;          if present, else nil.
  
(defun lm:Itemp ( coll item )
  ;; Lee Mac  ~  11.06.10
  (if
    (not
      (vl-catch-all-error-p
        (setq item
          (vl-catch-all-apply
            (function vla-item) (list coll item)
          )
        )
      )
    )
    item
  )
)

;; SelectifFoo (Lee Mac)
;; Continuous object selection prompt until
;; the predicate function 'foo' returns true.
;;
;; Arguments:
;; foo  ~  predicate function taking ename argument.
;; str  ~  prompt string.
;; Returns: return value of foo.
  
(defun lm:SelectifFoo ( foo str / sel ent )
  ;; Lee Mac  ~  11.06.10
  (while
    (progn
      (setq sel (entsel str))
      
      (cond
        (
          (vl-consp sel)

          (if (not (setq ent (foo (car sel))))
            (princ "\n** Invalid Object Selected **")
          )
        )
      )
    )
  )
  ent
)

;; Matrix x Vector  ~  Vladimir Nesterovsky
(defun mxv ( mat vec )
  (mapcar '(lambda ( row ) (apply '+ (mapcar '* row vec))) mat))

;; Matrix x Matrix  ~  Vladimir Nesterovsky
(defun mxm ( m q )
  (mapcar (function (lambda ( r ) (mxv (trp q) r))) m)
)

;; Matrix Transpose  ~  Doug Wilson
(defun trp ( m ) (apply 'mapcar (cons 'list m)))





;*********************** 모든설정 초기화 = DDS  *********************************************

(defun c:DDS()
   (setvar "BLIPMODE" 0) ;마우스 포인트에 점 찍히는 것 해제
   (setvar "INSUNITS" 6) ;도면 단위 m로 설정, mm는 4
   (setvar "FILEDIA" 1) ;열기, 저장 등 대화상자가 나오도록
   (setvar "NOMUTT" 0) ;명령창에 메세지 표시가 나오도록
   (setvar "CMDECHO" 1) ;명령창에 메세지 표시가 나오도록
   (setvar "MBUTTONPAN" 1) ;마우스 휠로 초점이동(PAN)이 되도록
   (setvar "FILLMODE" 1) ;2차원 솔리드 및 두께 있는 폴리선 속 채우기
   (setvar "HIGHLIGHT" 1) ;객체 선택시 하일라이트 되도록
   (setvar "PICKADD" 1) ;객체 여러개 선택시 하나만 선택 (Shift 눌러야 추가되는것) 해지
   (setvar "MIRRTEXT" 0) ;문자 MIRROR할 때 문자 방향 유지
   (setvar "PICKFIRST" 1) ;객체를 먼저 선택 후 명령이 입력되어도 되도록
   (setvar "PROXYNOTICE" 0) ;프록시객체 경고창 안나오도록
   (setvar "DBLCLKEDIT" 1) ;블록 더블 클릭 편집되도록
   (setvar "BLOCKEDITLOCK" 0) ;블록 더블 클릭 편집되도록
   (setvar "ZOOMFACTOR" 60) ;마우스 휠로 줌할때 확대/축소량 기본값
   (command "DRAGMODE" "A") ;객체를 움직일 때 가상 객체가 보이도록
   (command "UCSICON" "ON") ;UCS ICON 켜기 
   (command "CELWEIGHT" "-1") ;선가중치 BYLAYER로
   (command "CLAYER" "0") ;현재 레이어를 0 으로
   (command "-COLOR" "BYLAYER") ;현재 색상을 BYLAYER로
   (command "-LINETYPE" "S" "BYLAYER" "") ;현재 선종류를 BYLAYER로
   (princ)
)




;;*********************** 다중블럭 깨기 = BLC *********************************************

(defun c:blc (/ nam b en ss old0 os blss bname blp old70 old10 old8 old41 old50 ent tempindex index entlists insp tbp1 tbp2 base10 entlist addents)
(defun dtr (a) (* pi (/ a 180.0)))
(defun rtd (a) (/ (* a 180.0) pi))
(defun dxf (n ed) (cdr (assoc n ed)))

  (setq ss (entsel "\n 블럭선택 : "))
  (if ss
    (progn
      (command "undo" "g")
      (setq os (getvar "osmode"))
      (setvar "osmode" 0)
      (setq bnam (dxf 2 (entget (car ss))))
      (setq old10 (dxf 10 (entget (car ss))))
      (setq old8 (dxf 8 (entget (car ss))))
      (setq old50 (dxf 50 (entget (car ss))))
      (setq old70 (dxf 70 (entget (car ss))))
      (setq old41 (dxf 41 (entget (car ss))))
      (setq bname "sampless")
      (if (setq b (tblsearch "block" bnam))
	(if (setq en (dxf -2 b))
	  (progn
	    (setq blp (cdr (assoc 10 b)))
	    (setq ent (entget en))
	    (entmake
	      (append
		'((0 . "BLOCK"))
		(list (cons 2 bname))
		(list (cons 70 0))
		(list (cons 10 blp))
              )
	    )
	    (setq addents (addent ent))
	    (if addents
	      (progn
		(while (setq en (entnext en))
		  (setq ent (entget en))
		  (setq addents (addent ent))
		)
	      )
	    )
	    (setq bname (entmake '((0 . "ENDBLK"))))
	    (setq bname "sampless")
	    (setq ss (ssget "x" (list (cons 2 bnam))))
	    (if ss
	      (progn
		(setq index 0)
		(repeat (sslength ss)
		  (setq ssent (ssname ss index))
		  (setq ent (entget ssent))
		  (setq old8 (cdr (assoc 8 ent)))
		  (setq old10 (cdr (assoc 10 ent)))
		  (setq old41 (cdr (assoc 41 ent)))
		  (setq old42 (cdr (assoc 42 ent)))
		  (setq old43 (cdr (assoc 43 ent)))
		  (setq old50 (cdr (assoc 50 ent)))
		  (entdel ssent)
		  (entmake
		    (append
		      '((0 . "INSERT"))
		      (list (cons 2 bname))
		      (list (cons 10 old10))
		      (list (cons 41 old41))
		      (list (cons 42 old42))
		      (list (cons 43 old43))
		      (list (cons 50 old50))
		    )
		  )
		  (setq index (1+ index))
		)
		(command "purge" "b" bnam "n")
		(command "rename" "b" bname bnam)
              )
	    )
	  )
	)
      )
      (command "undo" "e")
    )
    (setvar "osmode" os)
  )
  (princ)
)

(defun addent (ent / index removenum entlist tempindex)
  (setq index 0)
  (setq removenum (list -1 330 5))
  (setq entlist nil)
  (repeat (length ent)
    (setq tempindex (car (nth index ent)))
    (if (not (member tempindex removenum))
      (progn
	(setq entlist (append entlist (list (nth index ent))))
      )
    )
    (setq index (1+ index))
  )
  (entmake
    (append
      (mapcar
	'(lambda (x)
	   x
	 )
	entlist
      )
    )
  )
  t
)



;;*********************** 숫자 증감시키기 = NAD1 *********************************************

(defun c:nad1(/ ss num k ed txt n numt q len @1 @2 txt1 txt2 txt3 newtxt)
 (prompt "\n>> 입력수치로 숫자만 증감시키기") 
 (setq ss (ssget (list (cons 0 "text,mtext"))))
 (if (not $num) (setq $num 1))
 (setq num (getint (strcat "\n증가할 수치<" (rtos $num 2 0) ">:")))
 (if num (setq $num num))
 (setq k 0)
 (repeat (sslength ss)
  (setq ed (entget (ssname ss k)))
  (setq txt (cdr (assoc 1 ed)))
  (setq n 1 numt "" q 0 len (strlen txt))
  (repeat len
   (setq @1 (substr txt n 1))
   (setq @2 (ascii @1))
   (if (and (>= @2 48) (<= @2 57))
    (progn
     (setq numt (strcat numt @1))
     (setq q n)
    )
   )
   (setq n (1+ n))
  )
  (setq txt1 (substr txt 1 (- q (strlen numt))))
  (setq txt2 (rtos (+ (atof numt) $num) 2 0))
  (setq txt3 (substr txt (1+ q) (- len q)))
  (setq newtxt (strcat txt1 txt2 txt3))
  (entmod (subst (cons 1 newtxt) (assoc 1 ed) ed))
  (setq k (1+ k))
 )
(princ)
) 




;;*********************** DWING PURGE = DP *********************************************

(defun c:DP(/ dcl_id tg1 tg2 tg3 tg4 tg5 tg6 tg7 ky fn fname)

(vl-load-com)
(create_dialog)

(prompt "\n** 도면 정리 하기 **\n\n")

(setq dcl_id (load_dialog fname))

(setq ky 4 tg1 "0" tg2 "0" tg3 "0" tg4 "1" tg5 "1" tg6 "1" tg7 "0")

(if (not (new_dialog "temp" dcl_id)) (exit) );if

   (set_tile "tog1" tg1)
   (set_tile "tog2" tg2)
   (set_tile "tog3" tg3)
   (set_tile "tog4" tg4)
   (set_tile "tog5" tg5)
   (set_tile "tog6" tg6)
   (set_tile "tog7" tg7)

   (action_tile "tog1" "(setq tg1 $value)")
   (action_tile "tog2" "(setq tg2 $value)")
   (action_tile "tog3" "(setq tg3 $value)")
   (action_tile "tog4" "(setq tg4 $value)")
   (action_tile "tog5" "(setq tg5 $value)")
   (action_tile "tog6" "(setq tg6 $value)")
   (action_tile "tog7" "(setq tg7 $value)(@ff_select tg7)")
   (action_tile "accept" "(setq ky 9)(done_dialog)")
   (action_tile "cancel" "(done_dialog)")

(start_dialog)
(unload_dialog dcl_id)

  (if (= ky 9)(progn
     (command "undo" "be")
     (if (= tg1 "1") (command "audit" "y"))
     (if (= tg2 "1") (@do))
     (if (= tg3 "1") (command "purge" "a" "*" "n"))
     (if (= tg4 "1") (@pointdelete))
     (if (= tg5 "1") (@layerfilterdelete))
     (if (= tg6 "1") (@ghost))
     (command "undo" "e")
     (prompt "\n\n** 도면정리완료 **"))
     (prompt "\n\n** 도면정리취소 **"))

(vl-file-delete fname)
(prin1))


;;subroutine (서브루틴)
(defun @ff_select(tg7)
   (cond
      ((= tg7 "0")(setq tg1 "0" tg2 "0" tg3 "0" tg4 "0" tg5 "0" tg6 "0"))
      ((= tg7 "1")(setq tg1 "1" tg2 "1" tg3 "1" tg4 "1" tg5 "1" tg6 "1"))
   )
   (set_tile "tog1" tg1)(set_tile "tog2" tg2)
   (set_tile "tog3" tg3)(set_tile "tog4" tg4)
   (set_tile "tog5" tg5)(set_tile "tog6" tg6)(set_tile "tog7" tg7)
)


;;솔리드 뒤로 보내기(2007.07.28 주말농부)
(defun @do(/ ss)
;   (prompt "\n ** 솔리드 뒤로 보내기 **\n\n")
   (setq ss (ssget "x" '((0 . "HATCH,TRACE"))) )
   (vl-cmdf "draworder" ss "" "b")
   (setq ss (ssget "x" '((0 . "TEXT,MTEXT,DIMENSION"))) )
   (vl-cmdf "draworder" ss "" "f")
   (prompt "\n솔리드을 뒤로 텍스트을 앞으로 보냄.\n")
(prin1))


;;레이어 필터 삭제하기
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
   (cond (i (princ (strcat "\n" (itoa i) " filters deleted.")))))
 ;-->ghost start
(defun @ghost(/ k j ss ss1 en ed etn x10 x11 lis n dissum dis
                tk tnum tss ten ted ttxt tvar @1 ttk)
   (setvar "cmdecho" 0)
   (prompt "\n내용이 없는 유형객체(text/mtext/line/pline)를 삭제함")
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
;
(defun GetPolyVtx(EntList)
   (setq VtxList '())
   (foreach x EntList
      (if (= (car x) 10) (setq VtxList (append VtxList (list (cdr x)))) )
   )
VtxList)
;-->ghost end
;; pointdelete
(defun @pointdelete(/ ss)
   (setq ss (ssget "x" (list (cons 0 "point"))))
   (if ss (progn (command "erase" ss "") (princ "\n->Point<node> ")
         (princ (sslength ss)) (princ "개") (princ " 삭제하였습니다.\n") ))
(prin1))


;; ------------------------------------------------------ dcl start -----------------------------
(defun create_dialog ()
(setq fname (vl-filename-mktemp "dcl.dcl"))
(setq fn (open fname "w"))
(write-line "temp
: dialog { label=\"도면 정리 하기\";
   : boxed_column {
   label = \"실행할 명령 선택\";
        : column {
            : toggle {
                   label=\" 도면감사<Audit> 하기\";
                   key=\"tog1\";
           }
            : toggle {
                   label=\" 솔리드 뒤로<Draworder> 보내기\";
                   key=\"tog2\";
            }
            : toggle {
                   label=\" 퍼지<Purge> 하기\";
                   key=\"tog3\";
            }
            : toggle {
                   label=\" 포인트<Node> 삭제\";
                   key=\"tog4\";
            }
            : toggle {
                   label=\" 레이어 필터<Filter> 삭제\";
                   key=\"tog5\";
            }
            : toggle {
                   label=\" 내용이 없는<Text>, 길이가 0인<Line> 유령객체 삭제\";
                   key=\"tog6\";
            }
           : toggle {
                   label=\" 모두 선택하기 / 모두 취소하기\";
                   key=\"tog7\";
            }
      }
   }
   ok_cancel;}
" fn)
(close fn)

);defun
(princ)

;; ------------------------------------------------------ dcl end -----------------------------



(defun c:qt (/ p1 ent p2 p3 p4 p5 ent op obj ang sset a)
  (vl-load-com)
  (command "undo" "be")
  (setq a (getvar "osmode"))
  (setvar "osmode" 0)
  (cond
    ((vl-cmdf "_.line" (setq p1 (getpoint "\nSelect First Point: "))
	      (setq p2 (getpoint p1 "\nSelect Second Point: "))
	      ""
     )
      (setq ent (entlast))
      (setq p3 (getpoint "\nWhich sides: "))
      (setq obj (vlax-ename->vla-object ent))
      (setq op (vlax-curve-getclosestpointto obj p3))
      (setq ang (angle op p3))
      (setq p4 (polar p1 ang 1))
      (setq p5 (polar p2 ang 1))
      (setq sset (ssget "_f" (list p1 p2)))
      (command "_.trim" sset "" "f" p4 p5 "" "")
      (command "_.erase" ent "")
    )
  )
  (setvar "osmode" a)
  (command "undo" "e")
)








******************** DIC : 균등분할 카피 *******************************************************

;;===========================================================
;  Divide copy
;  ->두점을 일정간격으로 나누어 선택한 Entity을 카피함
;;-----------------------------------------------------------
(defun c:DIC(/ myerror choi d-ss pt1 pt2 a1 d-t d-1 tot num)
;start --- Internal error handler ----------
 (defun myerror(S)
 (if (/= s "Function cancelled")(princ (strcat "\nError:" s)))
 (setvar "osmode" choi)
 (setq *error* olderr)(princ) )
 (setq olderr *error* *error* myerror)
;end----------------------------------------
   (setq choi (getvar "osmode"))
   (setq d-ss (ssget))
   (setvar "osmode" 32)
   (prompt "\nCommand: Divide Copy...")
   (setq pt1 (getpoint "\nBase point-> int of "))
   (setq pt2 (getpoint pt1 "\nSecond point-> int of"))
   (setvar "osmode" 0)
   (setq num (getint "\nNumber of divide=>"))
   (setq a1 (angle pt1 pt2) d-t (distance pt1 pt2)
         d-1 (/ d-t num)    tot d-1)
   (repeat (- num 1)
      (setq p1 (polar pt1 a1 tot))
      (command "copy" d-ss "" pt1 p1)
      (setq tot (+ tot d-1))
   )
   (setvar "osmode" choi)
   (prompt "\nResult | ")
   (prin1 d-1)(prompt " * ")(prin1 num)(prompt " = ")(prin1 d-t)
   (prin1)
)



******************** AIC : 아크 균등분할 *******************************************************

;;========================================================
; ARC DIVIDE (9307cho_i)
;;========================================================
(defun c:AIC (/ ss cenpt secondpt bang sang rang tang num)
   (prompt "\nCommand: Arc Divide...")
   (prompt "\nSelect object->")
   (setq ss (ssget))
   (setq cenpt (getpoint "\nEnter center point->"))
   (setq basept (getpoint "\nEnter first point->"))
   (setq secondpt (getpoint basept "\nEnter second point->"))
   (setq num (getint "\nNumber of divide =>"))
   (setq bang (angle cenpt basept))
   (setq sang (angle cenpt secondpt))
   (if (< bang sang)
       (progn
          (setq rang (- sang bang))
          (setq rang (rtd rang))
          (if (> rang 180)
              (setq rang (- rang 360))
          )
       (setq rang (/ rang num))
       )
   )
   (if (> bang sang)
       (progn
          (setq rang (- bang sang))
          (setq rang (rtd rang))
          (setq rang (- 0 rang))
          (if (< rang (- 0 180))
              (setq rang (+ 360 rang))
          )
       (setq rang (/ rang num))
       )
   )
   (setq num (- num 1))
   (repeat num
      (command "copy" "p" "" cenpt cenpt)
      (command "rotate" "p" "" cenpt rang)
   )
   (redraw)
   (setq num (+ num 1))
   (setq tang (* rang num))
   (prompt "\nResult | ")
   (prin1 rang)(prompt " * ")(prin1 num)(prompt " = ")(prin1 tang)
   (prin1)
)



********************* REV : 구름 마크 그리기 ******************************************************

(defun c:rev (/ ds plw pt1 pt2 p1 p2 xdist ydist spcsx spcsy ent1 ent2 nxt info bulge data c_o c_l)
	(print ">> Cloud Draw Tool...")
              (setq c_o (getvar "osmode"))
	(setvar "cmdecho" 0)
	(setvar "osmode" 0)
              (setq c_l (getvar "clayer")) ;<= 현재 레이어값을 저장한다.
              (setq ly (tblsearch "layer" "Revision")) ;"레이어를 검색 없으면 레이어 생성
              (if (= ly nil) (command "layer" "n" "Revision" ""))
              (setvar "clayer" "Revision") ;<= 현재 레이어를 변경한다.
		(setq	ds (getvar "dimscale")
		plw (* 0.02 ds)                                            
		oer *error*
		bm (getvar "blipmode"))
	(setq   ds (* 30 ds))
	(print)                                            ; ------------> circle size 
	(setq str (strcat "호 지름<" (rtos ds 2) "> : "))
	(setq  buf (getint str)) 
	(if (/= buf NIL) (setq ds buf))
	(defun *error* (s)						;start error routine
		(setvar "blipmode" bm)					;reset blipmode
		(princ (strcat "\Exit..." s))				;type error message
		(if oer (setq *error* oer))
		(princ))
	(print)
	(SETQ PT1 (GETPOINT "영역의 좌측 하단점: "))	(terpri)
	(setq pt2 (getcorner pt1 "영역의 우측 상단점: "))
	(setvar "blipmode" 0)
	(setq	p1 (car pt1)	p2 (car pt2)				;find x distances
		xdist (- p2 p1))
	(setq 	p1 (cadr pt1)	p2 (cadr pt2)				;find y distances
		ydist (- p2 p1))

;******TO ADJUST SPACING OF ARCS CHANGE THE NUMBER 2 IN THE NEXT TWO LINES*****
	(setq	spcsx (/ (abs xdist) (/ ds 2))				;X spacing
		spcsy (/ (abs ydist) (/ ds 2)))				;Y spacing
			
	(if (= spcsx (fix spcsx))	(setq spcsx (fix spcsx))	(setq spcsx (+ 1 (fix spcsx))))
	(if (= spcsx 1)	(setq spcsx 2))					;min of 2 spaces
	(if (= spcsy (fix spcsy))	(setq spcsy (fix spcsy))	(setq spcsy (+ 1 (fix spcsy))))
	(if (= spcsy 1)	(setq spcsy 2))					;min of 2 spaces
	
	(setq	xdist (/ xdist spcsx)	ydist (/ ydist spcsy))		;set distances

	(setq p1 pt1)							;set polyline start point
	
	(command "PLINE" p1 "W" "0" "")					;start polyline command
	(repeat spcsx							;draw bottom line segments
		(setq p1 (polar p1 0.0 (abs xdist)))
		(command p1))
	(repeat spcsy							;draw right line segments
		(setq p1 (polar p1 (/ pi 2) (abs ydist)))
		(command p1))
	(repeat spcsx							;draw top line segments
		(setq p1 (polar p1 pi (abs xdist)))
		(command p1))
	(repeat (- spcsy 1)						;draw left line segments
		(setq p1 (polar p1 (* pi 1.5) (abs ydist)))             
		(command p1))
	(command "C")							;Close polyline

	(setq	ent1 (entlast)						;get entity
		ent2 (entget ent1)					;get entity info
;******TO ADJUST THE ARC SIZE ADJUST THE 0.5 BELOW*******			
		bulge (list (cons 42 0.5))				;build cloud arcs   0.5
		nxt (cdr (assoc -1 ent2))				;set for lookup
		nxt (entnext nxt)					;get next one
		plw (list (cons 41 plw)))				;build cloud width

	(if (= nxt nil)
		(progn
			(setq ent2 (subst (cons 42 0.5) (assoc 42 ent2) ent2))
			(entmod ent2)							;modify entity
		)
		(while nxt							;start loop
			(setq	info (entget nxt)				;get exist. info
				info (append info bulge)			;set bulge
				info (append info plw)				;set width
			)							;end of setq
			(entmod info)							;modify entity
			(setq nxt (entnext nxt))				;get next segment
		)								;end of while
	)
	(entupd ent1)							;update entity

	(setvar "blipmode" bm)						;reset blipmode
	(setvar "cmdecho" 1)						;turn command echo on
              (setvar "osmode" c_o)
              (setvar "clayer" c_l) ;<= 저장된 값으로 현재 레이어를 변경한다.
	(gc) (princ)							;print blank line
)										;End program




;; Silent load.
(princ)
