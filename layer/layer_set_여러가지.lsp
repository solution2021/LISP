;layer BEAM
(defun c:plbox()
(setvar "cmdecho" 0)
(command "undo" "group")
(prompt "\nCommand: layer set PL_BOX(red) ....")
(setq en (ssget))
(command "layer" "m" "pl_box" "c" "red" "" "")
(command "change" en "" "p" "la" "pl_box" "c" "bylayer" "")
(command "undo" "en")
(princ)
)

;layer L1
(defun c:line1()
(setvar "cmdecho" 0)
(command "undo" "group")
(prompt "\nCommand: layer set L1(cyan) ....")
(setq en (ssget))
(command "layer" "m" "l1" "c" "cyan" "" "")
(command "change" en "" "p" "la" "l1" "c" "bylayer" "")
(command "undo" "en")
(princ)
)

;layer L2
(defun c:line2()
(setvar "cmdecho" 0)
(command "undo" "group")
(prompt "\nCommand: layer set L2(green) ....")
(setq en (ssget))
(command "layer" "m" "l2" "c" "green" "" "")
(command "change" en "" "p" "la" "l2" "c" "bylayer" "")
(command "undo" "en")
(princ)
)

;layer L3
(defun c:line3()
(setvar "cmdecho" 0)
(command "undo" "group")
(prompt "\nCommand: layer set L3(magenta) ....")
(setq en (ssget))
(command "layer" "m" "l3" "c" "6" "" "")
(command "change" en "" "p" "la" "l3" "c" "bylayer" "")
(command "undo" "en")
(princ)
)

;layer L4
(defun c:line4()
(setvar "cmdecho" 0)
(command "undo" "group")
(prompt "\nCommand: layer set L4(white) ....")
(setq en (ssget))
(command "layer" "m" "l4" "c" "7" "" "")
(command "change" en "" "p" "la" "l4" "c" "bylayer" "")
(command "undo" "en")
(princ)
)

;layer L5
(defun c:line5()
(setvar "cmdecho" 0)
(command "undo" "group")
(prompt "\nCommand: layer set L5(red) ....")
(setq en (ssget))
(command "layer" "m" "l5" "c" "1" "" "")
(command "change" en "" "p" "la" "l5" "c" "bylayer" "")
(command "undo" "en")
(princ)
)

;layer TEXT
(defun c:text1()
(setvar "cmdecho" 0)
(command "undo" "group")
(prompt "\nCommand: layer set TEXT ....")
(command "layer" "m" "text" "c" "yellow" "" "")
;(command "style" "HY울릉도L" "HY울릉도L" "" "" "" "" "")

   (setq ss (ssget)) (terpri)
(command "change" ss "" "p" "la" "text" "c" "bylayer" "")
   (setq n (sslength ss))
   (setq k 0)
   (while (<= 1 n)
       (setq en (ssname ss k))
       (if (eq (cdr (assoc 0 (entget en))) "TEXT")(progn
           (redraw en 3)
       (command "change" en "" "" "" "HY울릉도L" "" "" "")
           (redraw en 4)
       )   )
       (setq n (- n 1))
       (setq k (+ k 1))
   )
(COMMAND "UNDO" "EN")
   (princ)
)



;layer TEXT2
(defun c:text2()
(setvar "cmdecho" 0)
(command "undo" "group")
(prompt "\nCommand: layer set TEXT2 ....")
(command "layer" "m" "text2" "c" "7" "" "")
;(command "style" "HY울릉도L" "HY울릉도L" "" "" "" "" "")

   (setq ss (ssget)) (terpri)
   (command "change" ss "" "p" "la" "text2" "c" "bylayer" "")
   (setq n (sslength ss))
   (setq k 0)
   (while (<= 1 n)
       (setq en (ssname ss k))
       (if (eq (cdr (assoc 0 (entget en))) "TEXT")(progn
           (redraw en 3)
       (command "change" en "" "" "" "HY울릉도L" "" "" "")
           (redraw en 4)
       )   )
       (setq n (- n 1))
       (setq k (+ k 1))
   )
(COMMAND "UNDO" "EN")
   (princ)
)

;layer WALL1
(defun c:lWall1()
(setvar "cmdecho" 0)
(command "undo" "group")
(prompt "\nCommand: layer set WALL1 ....")
(setq en (ssget))
(command "layer" "m" "wall1" "c" "yellow" "" "")
(command "change" en "" "p" "la" "wall1" "c" "bylayer" "")
(command "undo" "en")
(princ)
)

;layer WALL2
(defun c:lWall2()
(setvar "cmdecho" 0)
(command "undo" "group")
(prompt "\nCommand: layer set WALL2 ....")
(setq en (ssget))
(command "layer" "m" "wall2" "c" "1" "" "")
(command "change" en "" "p" "la" "wall2" "c" "bylayer" "")
(command "undo" "en")
(princ)
)

;layer WALL3
(defun c:lWall3()
(setvar "cmdecho" 0)
(command "undo" "group")
(prompt "\nCommand: layer set WALL3 ....")
(setq en (ssget))
(command "layer" "m" "wall3" "c" "4" "" "")
(command "change" en "" "p" "la" "wall3" "c" "bylayer" "")
(command "undo" "en")
(princ)
)

;layer win
(defun c:lwin()
(setvar "cmdecho" 0)
(command "undo" "group")
(prompt "\nCommand: layer set WIN ....")
(setq en (ssget))
(command "layer" "m" "win" "c" "7" "" "")
(command "change" en "" "p" "la" "win" "c" "bylayer" "")
(command "undo" "en")
(princ)
)

;layer door
(defun c:ldoor()
(setvar "cmdecho" 0)
(command "undo" "group")
(prompt "\nCommand: layer set DOOR ....")
(setq en (ssget))
(command "layer" "m" "door" "c" "7" "" "")
(command "change" en "" "p" "la" "door" "c" "bylayer" "")
(command "undo" "en")
(princ)
)

;layer Block
(defun c:lblock()
(setvar "cmdecho" 0)
(command "undo" "group")
(prompt "\nCommand: layer set BLOCK ....")
(setq en (ssget))
(command "layer" "m" "block" "c" "1" "" "")
(command "change" en "" "p" "la" "BLOCK" "c" "bylayer" "")
(command "undo" "en")
(princ)
)

;layer center
(defun c:lcenter()
(setvar "cmdecho" 0)
(command "undo" "group")
(prompt "\nCommand: layer set CENTER ....")
(setq en (ssget))
(command "layer" "m" "center" "lt" "lisp-cen2" "" "c" "8" "" "")
(command "change" en "" "p" "la" "center" "c" "bylayer" "")
(command "undo" "en")
(princ)
)

;layer hatch
(defun c:lhatch()
(setvar "cmdecho" 0)
(command "undo" "group")
(prompt "\nCommand: layer set HATCH ....")
(setq en (ssget))
(command "layer" "m" "hatch" "c" "5" "" "")
(command "change" en "" "p" "la" "hatch" "c" "bylayer" "")
(command "undo" "en")
(princ)
)

;layer hatch
(defun c:ldim()
(setvar "cmdecho" 0)
(command "undo" "group")
(prompt "\nCommand: layer set DIMENSION ....")
(setq en (ssget))
(command "layer" "m" "dim" "c" "9" "" "")
(command "change" en "" "p" "la" "dim" "c" "bylayer" "")
(command "undo" "en")
(princ)
)

(defun c:llead()
(setvar "cmdecho" 0)
(command "undo" "group")
(prompt "\nCommand: layer set LEADER ....")
(setq en (ssget))
(command "layer" "m" "lead" "c" "7" "" "")
(command "change" en "" "p" "la" "lead" "c" "bylayer" "")
(command "undo" "en")
(princ)
)

(defun c:lsite()
(setvar "cmdecho" 0)
(command "undo" "group")
(prompt "\nCommand: layer set SITE ....")
(setq en (ssget))
(command "layer" "m" "site" "lt" "lisp-site" "" "c" "3" "" "")
(command "change" en "" "p" "la" "site" "c" "bylayer" "")
(command "undo" "en")
(princ)
)

(defun c:lstair()
(setvar "cmdecho" 0)
(command "undo" "group")
(prompt "\nCommand: layer set STAIR ....")
(setq en (ssget))
(command "layer" "m" "stair" "c" "red" "" "")
(command "change" en "" "p" "la" "stair" "c" "bylayer" "")
(command "undo" "en")
(princ)
)

(defun c:lbeam()
(setvar "cmdecho" 0)
(command "undo" "group")
(prompt "\nCommand: layer set BEAM ....")
(setq en (ssget))
(command "layer" "m" "beam" "c" "green" "" "")
(command "change" en "" "p" "la" "beam" "c" "bylayer" "")
(command "undo" "en")
(princ)
)

;250
(defun c:l250()
(setvar "cmdecho" 0)
(command "undo" "group")
(prompt "\nCommand: layer set 250 ....")
(setq en (ssget))
(command "layer" "m" "250" "c" "250" "" "")
(command "change" en "" "p" "la" "250" "c" "bylayer" "")
(command "undo" "en")
(princ)
)

;251
(defun c:l251()
(setvar "cmdecho" 0)
(command "undo" "group")
(prompt "\nCommand: layer set 251 ....")
(setq en (ssget))
(command "layer" "m" "251" "c" "251" "" "")
(command "change" en "" "p" "la" "251" "c" "bylayer" "")
(command "undo" "en")
(princ)
)

;252
(defun c:l252()
(setvar "cmdecho" 0)
(command "undo" "group")
(prompt "\nCommand: layer set 252 ....")
(setq en (ssget))
(command "layer" "m" "252" "c" "252" "" "")
(command "change" en "" "p" "la" "252" "c" "bylayer" "")
(command "undo" "en")
(princ)
)

;253
(defun c:l253()
(setvar "cmdecho" 0)
(command "undo" "group")
(prompt "\nCommand: layer set 253 ....")
(setq en (ssget))
(command "layer" "m" "253" "c" "253" "" "")
(command "change" en "" "p" "la" "253" "c" "bylayer" "")
(command "undo" "en")
(princ)
)

;254
(defun c:l254()
(setvar "cmdecho" 0)
(command "undo" "group")
(prompt "\nCommand: layer set 254 ....")
(setq en (ssget))
(command "layer" "m" "254" "c" "254" "" "")
(command "change" en "" "p" "la" "254" "c" "bylayer" "")
(command "undo" "en")
(princ)
)

;255
(defun c:l255()
(setvar "cmdecho" 0)
(command "undo" "group")
(prompt "\nCommand: layer set ROAD ....")
(setq en (ssget))
(command "layer" "m" "road" "c" "9" "" "")
(command "change" en "" "p" "la" "road" "c" "bylayer" "")
(command "undo" "en")
(princ)
)


(defun c:bylayer()
(setvar "cmdecho" 0)
(prompt "\n....Change bylayer")
(setq ss1 (ssget))
(command "change" ss1 "" "p" "c" "byl" "")
(princ)
)


(defun c:no_plot()
(setvar "cmdecho" 0)
;(prompt "\n....Layer NO PLOT")
(Setq ln (cdr (assoc 8 (entget (car (entsel))))))
(command "layer" "s" ln "p" "n" "" "")
(princ "\nLayer NO PLOT: ")
(princ ln) 
)


;;Layer Change Main PRG.('92-'01.03cho_i)
(defun c:ch_layer(/ ss ss1 lname lcc)
    (prompt "\nCommand: Layer & Color Change...")
    (prompt "\n>>Select objects to change ...:")
    (setq ss (ssget))
    (setq ss1 (entsel "\n>>Point to entity on target layer<layer list>:"))
    (if (= ss1 nil)
       (lc_box)
       (progn
           (setq lname (cdr (assoc 8 (entget (car ss1)))))
           (setq lcc (cdr (assoc 62 (entget (car ss1)))))
           (if (or (= lcc 0) (= lcc nil)) (setq lcc "bylayer") )
           (command "change" ss "" "p" "la" lname "c" lcc "")
      (SETVAR "OSMODE" 2199)
    ) )
   (princ)
)
;레이어의 목록을 만드는 리습
(defun @la_lst(/ cl k1 k2 la1 la2 la3)
   (setq cl (getvar "clayer") k1 0 k2 1)
   (setq la1 (tblnext "layer" t))
   (setq la1 (cdr (cadr la1)));layer name
   (setq la2 (tblnext "layer"))
   (if (/= la2 nil)(progn
       (setq la2 (cdr (cadr la2)));layer name
       (setq la-lst (list la2 la1))
       (if (= la2 cl) (setq k1 1))
   ))
   (setq la3 (tblnext "layer"))
   (while la3
       (setq la3 (cdr (cadr la3)));layer name
       (setq la-lst (cons la3 la-lst))
       (setq k2 (+ k2 1))
       (if (= la3 cl) (setq k1 k2))
       (setq la3 (tblnext "layer"))
   )
   (setq ck (itoa k1))
   (setq la-lst (reverse la-lst))
   (princ)
)
;dcl box을 가동시키는 리습
(defun lc_box(/ lno lname no ck la-lst)
   (@la_lst)
   (setq dcl_id (load_dialog "d:/cad-lisp/dcl/ch_layer.dcl"))
   (if (not (new_dialog "lay_lst" dcl_id)) (exit))
   (set_tile "k_lst" ck)
   (start_list "k_lst" 3)(mapcar 'add_list la-lst)(end_list)
   (action_tile "k_lst" "(setq lno $value)")
   (start_dialog)(terpri)
   (if (/= lno nil)(progn
       (setq no (atoi lno))
       (setq lname (nth no la-lst))
       (command "change" ss "" "p" "la" lname "")      
(SETVAR "OSMODE" 2199)
   ))
)


;;=====================================
;  Select Lock(0004cho_i)
;;-------------------------------------
(defun c:layer_lock(/ ss n k en cly oly)
  (setvar "cmdecho" 0)
   (prompt "\nCommand: Select Lock...")
   (prompt "\nLock layer select object... : ")
   (setq ss (ssget))
   (setq n (sslength ss))
   (setq k 0 tn n)
   (setq cly (cdr (assoc 8 (entget (ssname ss (1- n))))))
   (while (<= 1 n)
       (setq en (ssname ss k))
       (setq oly (cdr (assoc 8 (entget en))))
       (command "layer" "lo" oly "")
       (setq n (- n 1))
       (setq k (+ k 1))
    )
    (princ)
)



;;=====================================
;  Select Unlock(0004cho_i)
;;-------------------------------------
(defun c:layer_unlock(/ ss n k en cly oly)
   (setvar "cmdecho" 0)
   (prompt "\nCommand: Select Unlock...")
   (prompt "\nUnlock layer select<*.*>: ")
   (setq ss (ssget))
   (if (= ss nil) (progn
       (command "layer" "u" "*" "")
       (prompt "\nAll layer is unlock")
   ))
   (if (/= ss nil)(progn
     (setq n (sslength ss))
     (setq k 0 tn n)
     (setq cly (cdr (assoc 8 (entget (ssname ss (1- n))))))
     (while (<= 1 n)
       (setq en (ssname ss k))
       (setq oly (cdr (assoc 8 (entget en))))
       (command "layer" "u" oly "")
       (setq n (- n 1))
       (setq k (+ k 1))
     )
    ))
    (princ)
)




;; hatch Draw
(defun c:ll()
    (graphscr)
    (setvar "cmdecho" 0)
    (layer_dcl)
      (cond ((= ll_1 "1") (ll-1))
            ((= ll_2 "1") (ll-2))
            ((= ll_3 "1") (ll-3))
            ((= ll_4 "1") (ll-4))
            ((= ll_5 "1") (ll-5))
            ((= ll_6 "1") (ll-6))
            ((= ll_7 "1") (ll-7))
            ((= ll_8 "1") (ll-8))
            ((= ll_9 "1") (ll-9))
            ((= ll_10 "1") (ll-10))
            ((= ll_11 "1") (ll-11))
            ((= ll_12 "1") (ll-12))
            ((= ll_13 "1") (ll-13))
            ((= ll_14 "1") (ll-14))
            ((= ll_15 "1") (ll-15))
            ((= ll_16 "1") (ll-16))
            ((= ll_17 "1") (ll-17))
            ((= ll_18 "1") (ll-18))
            ((= ll_19 "1") (ll-19))
            ((= ll_20 "1") (ll-20))
            ((= ll_21 "1") (ll-21))
            ((= ll_22 "1") (ll-22))
            ((= ll_23 "1") (ll-23))
            ((= ll_24 "1") (ll-24))
            ((= ll_25 "1") (ll-25))
            ((= ll_26 "1") (ll-26))
            ((= ll_27 "1") (ll-27))
            ((= ll_28 "1") (ll-28))
            ((= ll_29 "1") (ll-29))
            ((= ll_30 "1") (ll-30))
      );cond
);defun

(defun ll-1()     (c:lwall1)   );defun
(defun ll-2()     (c:lwall2)   );defun
(defun ll-3()     (c:lwall3)   );defun
(defun ll-4()     (c:text1)   );defun
(defun ll-5()     (c:text2)   );defun
(defun ll-6()     (c:line1)   );defun
(defun ll-7()     (c:line2)   );defun
(defun ll-8()     (c:line3)   );defun
(defun ll-9()     (c:line4)   );defun
(defun ll-10()    (c:line5)   );defun
(defun ll-11()    (c:lwin)   );defun
(defun ll-12()    (c:ldoor)   );defun
(defun ll-13()    (c:lcenter)   );defun
(defun ll-14()    (c:lhatch)   );defun
(defun ll-15()    (c:ldim)   );defun
(defun ll-16()    (c:llead)   );defun
(defun ll-17()    (c:lblock)   );defun
(defun ll-18()    (c:lsite)   );defun
(defun ll-19()    (c:lstair)   );defun
(defun ll-20()    (c:lbeam)   );defun
(defun ll-21()    (c:l254)   );defun
(defun ll-22()    (c:l253)   );defun
(defun ll-23()    (c:l252)   );defun
(defun ll-24()    (c:l251)   );defun
(defun ll-25()    (c:l250)   );defun
(defun ll-26()    (c:plbox)   );defun
(defun ll-27()    (c:no_plot)   );defun
(defun ll-28()    (c:ch_layer)   );defun
(defun ll-29()    (c:layer_lock)   );defun
(defun ll-30()    (c:Layer_unlock)   );defun


;sub prg.
   (defun layer_dcl()
     (setq dcl_id (load_dialog "d:/cad-lisp/dcl/layer.dcl"))
     (if (not (new_dialog "layer_dcl" dcl_id)) (exit))

     (setq ll_1 "" ll_2 "" ll_3 "" ll_4 "" ll_5 "" ll_6 "" ll_7 "" ll_8 "" ll_9 "" ll_10 "" ll_11 "" ll_12 "" ll_13 "" 
ll_14 "" ll_15 "" ll_16 "" ll_17 "" ll_18 "" ll_19 "" ll_20 "" ll_21 "" ll_22 "" ll_23 "" ll_24 "" ll_25 "" ll_26 "" ll_27 "" ll_28 "" ll_29 "" ll_30 "")

     (setq x (dimx_tile "ll_1"))
     (setq y (dimy_tile "ll_1"))
     (start_image "ll_1")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/layer/w1")
     (end_image)

     (setq x (dimx_tile "ll_2"))
     (setq y (dimy_tile "ll_2"))
     (start_image "ll_2")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/layer/w2")
     (end_image)

     (setq x (dimx_tile "ll_3"))
     (setq y (dimy_tile "ll_3"))
     (start_image "ll_3")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/layer/w3")
     (end_image)

     (setq x (dimx_tile "ll_4"))
     (setq y (dimy_tile "ll_4"))
     (start_image "ll_4")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/layer/t1")
     (end_image)

     (setq x (dimx_tile "ll_5"))
     (setq y (dimy_tile "ll_5"))
     (start_image "ll_5")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/layer/t2")
     (end_image)

     (setq x (dimx_tile "ll_6"))
     (setq y (dimy_tile "ll_6"))
     (start_image "ll_6")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/layer/l1")
     (end_image)

     (setq x (dimx_tile "ll_7"))
     (setq y (dimy_tile "ll_7"))
     (start_image "ll_7")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/layer/l2")
     (end_image)

     (setq x (dimx_tile "ll_8"))
     (setq y (dimy_tile "ll_8"))
     (start_image "ll_8")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/layer/l3")
     (end_image)

     (setq x (dimx_tile "ll_9"))
     (setq y (dimy_tile "ll_9"))
     (start_image "ll_9")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/layer/l4")
     (end_image)

     (setq x (dimx_tile "ll_10"))
     (setq y (dimy_tile "ll_10"))
     (start_image "ll_10")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/layer/l5")
     (end_image)

     (setq x (dimx_tile "ll_11"))
     (setq y (dimy_tile "ll_11"))
     (start_image "ll_11")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/layer/w")
     (end_image)

     (setq x (dimx_tile "ll_12"))
     (setq y (dimy_tile "ll_12"))
     (start_image "ll_12")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/layer/d")
     (end_image)

     (setq x (dimx_tile "ll_13"))
     (setq y (dimy_tile "ll_13"))
     (start_image "ll_13")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/layer/ce")
     (end_image)

     (setq x (dimx_tile "ll_14"))
     (setq y (dimy_tile "ll_14"))
     (start_image "ll_14")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/layer/h")
     (end_image)

     (setq x (dimx_tile "ll_15"))
     (setq y (dimy_tile "ll_15"))
     (start_image "ll_15")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/layer/dim")
     (end_image)

     (setq x (dimx_tile "ll_16"))
     (setq y (dimy_tile "ll_16"))
     (start_image "ll_16")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/layer/le")
     (end_image)

     (setq x (dimx_tile "ll_17"))
     (setq y (dimy_tile "ll_17"))
     (start_image "ll_17")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/layer/b")
     (end_image)

     (setq x (dimx_tile "ll_18"))
     (setq y (dimy_tile "ll_18"))
     (start_image "ll_18")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/layer/site")
     (end_image)

     (setq x (dimx_tile "ll_19"))
     (setq y (dimy_tile "ll_19"))
     (start_image "ll_19")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/layer/st")
     (end_image)

     (setq x (dimx_tile "ll_20"))
     (setq y (dimy_tile "ll_20"))
     (start_image "ll_20")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/layer/beam")
     (end_image)

     (setq x (dimx_tile "ll_21"))
     (setq y (dimy_tile "ll_21"))
     (start_image "ll_21")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/layer/254")
     (end_image)

     (setq x (dimx_tile "ll_22"))
     (setq y (dimy_tile "ll_22"))
     (start_image "ll_22")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/layer/253")
     (end_image)

     (setq x (dimx_tile "ll_23"))
     (setq y (dimy_tile "ll_23"))
     (start_image "ll_23")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/layer/252")
     (end_image)

     (setq x (dimx_tile "ll_24"))
     (setq y (dimy_tile "ll_24"))
     (start_image "ll_24")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/layer/251")
     (end_image)

     (setq x (dimx_tile "ll_25"))
     (setq y (dimy_tile "ll_25"))
     (start_image "ll_25")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/layer/250")
     (end_image)

     (setq x (dimx_tile "ll_26"))
     (setq y (dimy_tile "ll_26"))
     (start_image "ll_26")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/layer/plbox")
     (end_image)

     (setq x (dimx_tile "ll_27"))
     (setq y (dimy_tile "ll_27"))
     (start_image "ll_27")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/layer/np")
     (end_image)

     (setq x (dimx_tile "ll_28"))
     (setq y (dimy_tile "ll_28"))
     (start_image "ll_28")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/layer/ch")
     (end_image)

     (setq x (dimx_tile "ll_29"))
     (setq y (dimy_tile "ll_29"))
     (start_image "ll_29")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/layer/lock")
     (end_image)

     (setq x (dimx_tile "ll_30"))
     (setq y (dimy_tile "ll_30"))
     (start_image "ll_30")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/layer/unlock")
     (end_image)


     (action_tile "ll_1" "(setq ll_1 $value)")
     (action_tile "ll_2" "(setq ll_2 $value)")
     (action_tile "ll_3" "(setq ll_3 $value)")
     (action_tile "ll_4" "(setq ll_4 $value)")
     (action_tile "ll_5" "(setq ll_5 $value)")
     (action_tile "ll_6" "(setq ll_6 $value)")
     (action_tile "ll_7" "(setq ll_7 $value)")
     (action_tile "ll_8" "(setq ll_8 $value)")
     (action_tile "ll_9" "(setq ll_9 $value)")
     (action_tile "ll_10" "(setq ll_10 $value)")
     (action_tile "ll_11" "(setq ll_11 $value)")
     (action_tile "ll_12" "(setq ll_12 $value)")
     (action_tile "ll_13" "(setq ll_13 $value)")
     (action_tile "ll_14" "(setq ll_14 $value)")
     (action_tile "ll_15" "(setq ll_15 $value)")
     (action_tile "ll_16" "(setq ll_16 $value)")
     (action_tile "ll_17" "(setq ll_17 $value)")
     (action_tile "ll_18" "(setq ll_18 $value)")
     (action_tile "ll_19" "(setq ll_19 $value)")
     (action_tile "ll_20" "(setq ll_20 $value)")
     (action_tile "ll_21" "(setq ll_21 $value)")
     (action_tile "ll_22" "(setq ll_22 $value)")
     (action_tile "ll_23" "(setq ll_23 $value)")
     (action_tile "ll_24" "(setq ll_24 $value)")
     (action_tile "ll_25" "(setq ll_25 $value)")
     (action_tile "ll_26" "(setq ll_26 $value)")
     (action_tile "ll_27" "(setq ll_27 $value)")
     (action_tile "ll_28" "(setq ll_28 $value)")
     (action_tile "ll_29" "(setq ll_29 $value)")
     (action_tile "ll_30" "(setq ll_30 $value)")

     (start_dialog)
     (done_dialog)
     (unload_dialog dcl_id)
     (princ)
   )
