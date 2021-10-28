(defun c:area2(/ dum ) 
 (setq a nil b nil)

 (defun *error* (s) 
 (if os (setvar "OSMODE" os))
 )
 (setq os (getvar "OSMODE"))

 (setq DS (getvar "DIMSCALE"))
 (setvar "osmode" 0)
 (setq asm 0)

 (while (setq poin (getpoint "\npick point:"))
 (SETQ A (ENTLAST))
 (COMMAND "undo" "group")
 (command "bpoly" poin "")
 (SETQ B (ENTLAST))
 (IF (/= (cdr (assoc 5 (entget a))) (cdr (assoc 5 (entget b)))) 
  (progn
   (grdrawx (entget b))
   (COMMAND "AREA" "O" B)
   (setq ar (getvar "area")
        asm (+ asm ar)
      apyng (* asm 0.3025)
)

    (COMMAND "ERASE" B "" )
    (princ (strcat "\n\n ▶Length  = "(rtos (/ (getvar "Perimeter") 1000) 2 4) " m 
                    \n\n ▶Area    = "(rtos (/ (getvar "area") 1000000) 2 4) "(m2)         "
                                      (rtos (/ (* (getvar "area") 0.3025) 1000000) 2 4) "(py)"  ) ) 
   (SETQ AR1 (/ AR 1000000.0))
   (SETQ AR2 (/ AR1 1.0))
   (setq ar3 (* ar1 0.3025))
;   (PROMPT "\nClick Text Point.......")(SETQ PO (GETPOINT))
   (SETQ TESI (* 2.5 DS))
;   (SETQ ART1 (STRCAT AR2))
;   (SETQ ART2 (STRCAT AR3))
   (COMMAND "TEXT" "br" POin tesi "" (strcat (rtos ar2 2 2)))
   (COMMAND "TEXT" "tr" POin TESI "" (strcat (rtos ar3 2 2)))
)
)
)



  (command "redraw")
  (setq pto (getpoint "\n TOTAL..... :"))
  (SETQ Asm1 (/ Asm 1000000.0))
  (SETQ apyng1 (/ apyng 1000000.0))
  (COMMAND "TEXT" "br" pto tesi "" (strcat (rtos asm1 2 2)))
  (COMMAND "TEXT" "tr" pto tesi "" (strcat (rtos apyng1 2 2)))
  (command "undo" "en")
  (setvar "osmode" 2199)

   (princ)
)



(defun grdrawx(a / TY B- C-)
(setq b- nil)
(while (setq c- (assoc 10 a))
(if (NOT b-)
(setq b- c- TY C-) 
(progn 
(grdraw (cdr b-) (cdr c-) 1 1)
(setq b- c-)
) 
) 
(setq a (cdr (member (assoc 10 a) a)))
)
(grdraw (cdr TY) (cdr B-) 1 1)
)


(DEFUN C:room_point(/ fp np ptlist no square os bl)
  (defun *error* (s) 
  (if os (setvar "OSMODE" os))
  )
  (setq os (getvar "OSMODE"))
  (setq bl (GETVAR "blipmode"))
  (setq DS (getvar "DIMSCALE"))
  (setvar "cmdecho" 0)
  (SETVAR "osmode" 161)
  (SETVAR "blipmode" 1)
  (SETVAR "orthomode" 0)
  (COMMAND "undo" "group")
  (PROMPT "\nCalculate Area...")
  (SETQ fp (GETPOINT "\nFirst point: ")
        ptlist (LIST fp))
  (WHILE (SETQ np (GETPOINT fp "\nNext point: "))
  (IF np (SETQ ptlist (APPEND ptlist (LIST np))  fp np))
  )
  (if (= fp "U")
    (progn
      (command "Undo"))) 
  (SETVAR "osmode" 0)
  (SETQ no 0)
  (COMMAND "Area")
    (REPEAT (LENGTH ptlist)
      (COMMAND (NTH no ptlist))
      (SETQ no (1+ no))
    )
  (COMMAND "")
       (SETQ TESI (* 2.5 DS))
       (setq ar (getvar "area")) 
       (setq le (getvar "dist"))
       (SETQ AR1 (/ AR 1000000.0))
       (PROMPT "▶LENGTH(m) = ")(prin1 (setq le2 (RTOS (/ le 1.0) 2 4)))(terpri)
       (prompt "▶AREA (㎡) = ")(prin1 (SETQ AR2 (RTOS (/ AR1 1.0) 2 4)))
       (PROMPT "       (py) = ")(prin1 (setq ar3 (RTOS (* ar1 0.3025) 2 4))) 
       (SETVAR "blipmode" 0)   
       (PROMPT "\nTEXT POINT.......")(SETQ PO (GETPOINT))

       (SETQ ART1 (STRCAT AR2))
       (SETQ ART2 (STRCAT AR3))
       (COMMAND "TEXT" "br" PO tesi "" art1)
       (COMMAND "TEXT" "tr" PO TESI "" art2)
  (command "redraw")
  (setvar "osmode" 2199)

  (COMMAND "undo" "en")
  (PRINC)
  )


(defun C:room_poly()
       (setvar "cmdecho" 0)
       (setq DS (getvar "DIMSCALE"))
       (SSGET)
       (command "AREA" "E" (SSGET "p"))
       (setq ar (getvar "area"))
       (SETQ AR1 (/ AR 1000000.0)) 
       (prompt "\n▶AREA(㎡) = ") (prin1 (SETQ AR2 (RTOS (/ AR1 1.0) 2 2))) 
       (PROMPT "\n      (py) = ") (prin1 (setq ar3 (RTOS (* ar1 0.3025) 2 2))) 
       (SETQ PO (GETPOINT "\nTEXT POINT......."))
       (SETQ TESI (* 2.5 DS))
       (SETQ ART1 (STRCAT AR2))
       (SETQ ART2 (STRCAT AR3))
       (COMMAND "TEXT" "br" PO tesi "" art1)
       (COMMAND "TEXT" "tr" PO TESI "" art2)
       (setvar "osmode" 2199)

)


;............................Multi Polyline Area Calculation
(defun c:room_total(/ cmd clr ort a pyng sm pt)
  (setvar "cmdecho" 0)
  (setq DS (getvar "DIMSCALE"))
  (command "undo" "group")
  (setq pl_line_w (ssget)
        pl_no (sslength pl_line_w)
        pl_line (ssname pl_line_w 0)
        ob_no 0
        asm 0
  );setq
  (while pl_line
;    (if (/= (cdr (assoc 0 (entget pl_line))) "POLYLINE")
;         (prompt "\n Not Polyline ! ")
         (progn
            (redraw pl_line 3)
            (command "area" "e" pl_line)
            (setq a (getvar "area")
                  sm (/ a 1.0e+06)
                  asm (+ asm sm)
                  pyng (* sm 0.3025)
                  pt (getpoint "\nPick Point... :")
            );setq
            (princ "\n..... Area : ")
            (princ sm)
            (princ " (㎡)   ;")
            (princ "            ")
            (princ pyng)
            (princ " (py)")
            (SETQ TESI (* 2.5 DS)) 
            (command "text" "br" pt TESI "" (strcat (rtos sm 2 2)))
            (if (/= (tblsearch "layer" "areat") nil)
                (command "chprop" "l" "" "la" "areat" "")
            );if
            (command "text" "tr" pt TESI "" (strcat (rtos pyng 2 2)))
            (if (/= (tblsearch "layer" "areat2") nil)
                (command "chprop" "l" "" "la" "areat2" "")
            );if 
            (redraw pl_line 4)
         );progn
;     );if
     (setq ob_no (1+ ob_no)
           pl_line (ssname pl_line_w ob_no)
     );setq
  );while
  (setq pt (getpoint "\nTOTAL..... :")
        apyng (* asm 0.3025)
  );setq
  (SETQ TESI (* 2.5 DS))
   (command "text" "br" pt TESI "" (strcat (rtos asm 2 2) " M2"))
  (if (/= (tblsearch "layer" "areat") nil)
      (command "chprop" "l" "" "la" "areat" "")
  );if
  (command "text" "tr" pt TESI "" (strcat (rtos apyng 2 2) " py"))
  (if (/= (tblsearch "layer" "areat2") nil)
      (command "chprop" "l" "" "la" "areat2" "")
  (command "undo" "en")
  );if
  (setvar "osmode" 2199)

  (princ)
)



(defun c:room_area (/ Sel EntData PolyObj TextObj ReactList Pos)
;   by T.Willey from http://www.theswamp.org
;|  Adds a presistant reactor to a polyline object that
    updates a selected text object to the polylines area
    in square feet.  You will have to have the subs loaded
    in everydrawing for it to work, so that it know what
    to do with the reactor, because it is saved with the
    drawing.  Saves the association between the text
    and the polyline in the extension dictionary of the
    polyline.  If the text object is deleted, then the
    program will remove the reactor related to the polyline.
    Thanks to Luis Esquivel for his help and guidance.
    v1.0 2/2006 Tim Willey
|;



       (setvar "cmdecho" 0)
       (setq DS (getvar "DIMSCALE"))
       (setq ss (entsel) en1 (car ss))   (redraw en1 3)
       (command "AREA" "E" ss)
       (setq ar (getvar "area"))
       (SETQ AR1 (/ AR 1000000.0))
       (prompt "\n▶AREA...(㎡) = ") (prin1 (SETQ AR2 (RTOS (/ AR1 1.0) 2 2))) 
       (PROMPT "\n      ...(py) = ") (prin1 (setq ar3 (RTOS (* ar1 0.3025) 2 2))) 
       (SETQ PO (GETPOINT "\nTEXT POINT......."))
       (SETQ TESI (* 2.5 DS))
       (SETQ ART1 (STRCAT AR2))
       (COMMAND "TEXT" "br" PO tesi "" art1)


(vl-load-com)
(if
 (and
;  (setq Sel (entsel "\n select polyline: "))
  (setq Sel ss)
  (setq EntData (entget (car Sel)))
  (= (cdr (assoc 0 EntData)) "LWPOLYLINE")
  (setq PolyObj (vlax-ename->vla-object (car Sel)))
  (setq Sel (entsel "\n select base TEXT: "))
  (setq EntData (entget (car Sel)))
  (vl-position (cdr (assoc 0 EntData)) '("TEXT" "MTEXT"))
  (setq TextObj (vlax-ename->vla-object (car Sel)))
 )
 (progn
  (PutArea PolyObj TextObj)
  (if
   (and
    (setq ReactList (AssociatedReactors PolyObj))
    (setq Pos (vl-position "MyAreaReactorModified" (mapcar 'vlr-data ReactList)))
   )
   (vlr-remove (nth Pos ReactList))
  )
  (vlr-pers
   (vlr-object-reactor
    (list PolyObj)
    "MyAreaReactorModified"
    '(
     (:vlr-modified . MakeCmdEndReactor)
     (:vlr-erased . ObjectEraseReactor)
;     (:vlr-unerased . ObjectUnErasedReactor)
    )
   )
  )
 )
)
(princ)
)
;---------------------------------------------------------------------------------------------------------------
(defun PutArea (PolyObj TextObj / Dict xRec SqFt)

(setq Dict (vla-GetExtensionDictionary PolyObj))
(if (vl-catch-all-error-p (setq xRec (vl-catch-all-apply 'vla-Item (list Dict "MyAreaReactor"))))
 (setq xRec (vla-AddXRecord Dict "MyAreaReactor"))
)
(MySetXrec xRec '(40 1) (list (vlax-get PolyObj 'Area) (vlax-get TextObj 'Handle)))
;;(setq SqFt (vla-get-Area PolyObj))
(setq SqFt (/ (vla-get-Area PolyObj) 1000000.0))                 ;;; 자릿수 변경
;;(vla-put-TextString TextObj (rtos SqFt 2 2))
(vla-put-TextString TextObj (strcat (rtos SqFt 2 2)"㎡"))       ;;; 단위
xRec
       (prompt "\n▶AREA(㎡) = ")(prin1 (RTOS (/ SqFt 1.0) 2 4))
       (PROMPT "\n      (py) = ")(prin1 (RTOS (* SqFt 0.3025) 2 4)) 
       (setvar "osmode" 2199)
)


;----------------------------------------------------------------------------------------------------------------
(defun MakeCmdEndReactor (Obj React NotSure)

(if (not (wcmatch (getvar "cmdnames") "U,UNDO,REDO,OOPS"))
 (progn
  (if GlbVarAreaObject
   (setq GlbVarAreaObject (append GlbVarAreaObject (list Obj)))
   (setq GlbVarAreaObject (list Obj))
  )
  (if (not GlbReactorCommandEnd)
   (setq GlbReactorCommandEnd (vlr-command-reactor "tempAreaCommandReactor" '((:vlr-commandEnded . AdjustTextObj))))
  )
 )
)
(princ)
)
;------------------------------------------------------------------------------------------------------------------
(defun ObjectEraseReactor (Obj React NotSure)

(vlr-pers-release React)
(vlr-remove React)
)
;-----------------------------------------------------------------------------------------------------------------
(defun ObjectUnErasedReactor (Obj React NotSure)

(vlr-pers
 (vlr-object-reactor
  (list Obj)
  "MyAreaReactorModified"
  '(
   (:vlr-modified . MakeCmdEndReactor)
   (:vlr-erased . ObjectEraseReactor)
   (:vlr-unerased . ObjectUnErasedReactor)
  )
 )
)
)
;-----------------------------------------------------------------------------------------------------------------
(defun AdjustTextObj (React CommandList / Dict xRec xRecList TextObj)

(foreach Obj GlbVarAreaObject
 (if (not (vlax-erased-p Obj))
  (progn
   (setq Dict (vla-GetExtensionDictionary Obj))
   (if (not (vl-catch-all-error-p (setq xRec (vl-catch-all-apply 'vla-Item (list Dict "MyAreaReactor")))))
    (progn
     (setq xRecList (MyGetXRec xRec))
     (if
      (and
       (setq TextObj (vlax-ename->vla-object (setq tmpEnt (handent (cdr (assoc 1 xRecList))))))
       (not (vlax-erased-p TextObj))
      )
      (PutArea Obj TextObj)
      (progn
       (foreach i (AssociatedReactors Obj)
        (if (= (vlr-data i) "MyAreaReactorModified")
         (progn
          (vlr-pers-release i)
          (vlr-remove i)
         )
        )
       )
       (prompt "\n Reactor has be removed because the text object has been erased.")
      )
     )
    )
   )
  )
 )
)
(setq GlbVarAreaObject nil)
(vlr-remove GlbReactorCommandEnd)
(setq GlbReactorCommandEnd nil)
)
;---------------------------------------------------------------------------
(defun MySetXRec (Obj CodeList DataList / )
; Sets XRecordData. Dxf numbers between 1-369, except 5, 100, 105.
; See help for types and numbers to use.

(vla-SetXRecordData Obj
 (vlax-make-variant
  (vlax-safearray-fill
   (vlax-make-safearray
    vlax-vbInteger
    (cons 0 (1- (length CodeList)))
   )
   CodeList
  )
 )
 (vlax-make-variant
  (vlax-safearray-fill
   (vlax-make-safearray
    vlax-vbVariant
    (cons 0 (1- (length Datalist)))
   )
   DataList
  )
 )
)
)
;-----------------------------------------------------------------------------
(defun MyGetXRec (Obj / CodeType DataType)
; Retrive XRecordData for an object

(vla-GetXRecordData
 Obj
 'CodeType
 'DataType
)
(if (and CodeType DataType)
 (mapcar
  '(lambda (a b)
   (cons a (variant-value b))
  )
  (safearray-value CodeType)
  (safearray-value DataType)
 )
)
)
;-------------------------------------------------------------------------------------
(defun AssociatedReactors (Obj / ReactList)
; Return a list of reactors (object type) associated with an object.
; Use like (AssociatedReactors (vlax-ename->vla-object (car (entsel))))

(foreach i (cdar (vlr-reactors :vlr-object-reactor))
 (if (vl-position Obj (vlr-owners i))
  (setq ReactList (cons i ReactList))
 )
)
ReactList
)
;---------------------------------------------------------------------------
(defun RemovePersReact ()
; Remove persistant reactors that don't have an owner.

(foreach i (vlr-pers-list)
 (if (not (vlr-owners i))
  (progn
   (vlr-pers-release i)
   (vlr-remove i)
  )
 )
)
)




;; Arrow Draw
(defun c:aa()
    (graphscr)
    (setvar "cmdecho" 0)
    (ro_dcl)
      (cond ((= ro_1 "1") (ro-1))
            ((= ro_2 "1") (ro-2))
            ((= ro_3 "1") (ro-3))
            ((= ro_4 "1") (ro-4))
      );cond
);defun

(defun ro-1()     (c:area2)       );defun
(defun ro-2()     (c:room_poly)   );defun
(defun ro-3()     (c:room_total)  );defun
(defun ro-4()     (c:room_area)   );defun

;sub prg.
   (defun ro_dcl()
     (setq dcl_id (load_dialog "d:/cad-lisp/dcl/area.dcl"))
     (if (not (new_dialog "area_dcl" dcl_id)) (exit))

     (setq ro_1 "" ro_2 "" ro_3 "" ro_4 "")

     (setq x (dimx_tile "ro_1"))
     (setq y (dimy_tile "ro_1"))
     (start_image "ro_1")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/area/r0")
     (end_image)

     (setq x (dimx_tile "ro_2"))
     (setq y (dimy_tile "ro_2"))
     (start_image "ro_2")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/area/r2")
     (end_image)

     (setq x (dimx_tile "ro_3"))
     (setq y (dimy_tile "ro_3"))
     (start_image "ro_3")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/area/r3")
     (end_image)

     (setq x (dimx_tile "ro_4"))
     (setq y (dimy_tile "ro_4"))
     (start_image "ro_4")
     (slide_image 0 0 x y "d:/cad-lisp/dcl/area/r1")
     (end_image)

     (action_tile "ro_1" "(setq ro_1 $value)")
     (action_tile "ro_2" "(setq ro_2 $value)")
     (action_tile "ro_3" "(setq ro_3 $value)")
     (action_tile "ro_4" "(setq ro_4 $value)")

     (start_dialog)
     (done_dialog)
     (unload_dialog dcl_id)
     (princ)
   )