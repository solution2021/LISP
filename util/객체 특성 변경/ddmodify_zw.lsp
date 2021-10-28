;--------------------------------ddmodify.lsp----------------------------------
; Next available MSG number is    111
; MODULE_ID DDMODIFY_LSP_
;;;----------------------------------------------------------------------------
;;;    DDMODIFY.LSP
;;;
;;;    Copyright 1997 by Autodesk, Inc.
;;;
;;;    Permission to use, copy, modify, and distribute this software
;;;    for any purpose and without fee is hereby granted, provided
;;;    that the above copyright notice appears in all copies and
;;;    that both that copyright notice and the limited warranty and
;;;    restricted rights notice below appear in all supporting
;;;    documentation.
;;;
;;;    AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
;;;    AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
;;;    MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.
;;;    DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
;;;    UNINTERRUPTED OR ERROR FREE.
;;;
;;;    Use, duplication, or disclosure by the U.S. Government is subject to
;;;    restrictions set forth in FAR 52.227-19 (Commercial Computer
;;;    Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii)
;;;    (Rights in Technical Data and Computer Software), as applicable.
;;;
;;;.
;;;
;;;----------------------------------------------------------------------------
;;;   DESCRIPTION
;;;
;;;   This function allows the user to get a listing comparable to the LIST
;;;   command for most objects.  In addition, most object fields in the
;;;   dialogue box are editable.  Points can be specified dynamically by
;;;   temporarily dismissing the dialogue box.  Each object has a unique
;;;   dialogue.
;;;
;;;   Naming conventions
;;;   Long function and widget names may use an underscore "_"
;;;   in their names to make them easier to read, long variable
;;;   names use a dash "-".
;;;----------------------------------------------------------------------------
;;;----------------------------------------------------------------------------
;;;   Prefixes in command and keyword strings:
;;;      "."  specifies the built-in AutoCAD command in case it has been
;;;           redefined.
;;;      "_"  denotes an AutoCAD command or keyword in the native language
;;;           version, English.
;;;----------------------------------------------------------------------------
;;;
;;; Avoid (gc)s on load to improve load time.
;;;
(defun do_alloc (/ old_allod new_alloc)
  (setq old_alloc (alloc 2000) new_alloc (alloc 2000))
  (expand (1+ (/ 17000 new_alloc)))
  (alloc old_alloc)
)
(do_alloc)
(setq do_alloc nil)

;;;
;;;
;;; ===========================================================================
;;; ===================== load-time error checking ============================

  (defun ai_abort (app msg)
     (defun *error* (s)
        (if old_error (setq *error* old_error))
        (princ)
     )
     (if msg
       (alert (strcat " Application error: "
                      app
                      " \n\n  "
                      msg
                      "  \n"
              )
       )
     )
     (exit)
  )

;;; Check to see if AI_UTILS is loaded, If not, try to find it,
;;; and then try to load it.  If it can't be found or can't be
;;; loaded, then abort the loading of this file immediately.

  (cond
     (  (and ai_dcl (listp ai_dcl)))          ; it's already loaded.

     (  (not (findfile "ai_utils_zw.lsp"))                     ; find it
        (ai_abort "DDMODIFY"
                  (strcat "Can't locate file AI_UTILS.LSP."
                          "\n Check support directory.")))

     (  (eq "failed" (load "ai_utils_zw" "failed"))            ; load it
        (ai_abort "DDMODIFY" "Can't load file AI_UTILS.LSP"))
  )

;;; If we get this far, then AI_UTILS.LSP is loaded and it can
;;; be assumed that all functions defined therein are available.

;;; Next, check to see if ACADAPP.EXP has been xloaded, and abort
;;; if the file can't be found or xloaded.  Note that AI_ACADAPP
;;; does not abort the running application itself (so that it can
;;; also be called from within the command without also stopping
;;; an AutoCAD command currently in progress).

  (if (not (ai_acadapp))               ; defined in AI_UTILS.LSP
      (ai_abort "DDMODIFY" nil)        ; a Nil <msg> supresses
  )                                    ; ai_abort's alert box dialog.

;;; ==================== end load-time operations ===========================

;;; global variables

(setq hatch-elist nil)

;;; If we get this far, both ai_utils.lsp and acadapp.exp are
;;; assumed to be available.

;;; Define and encapsulate all subroutines that are declared
;;; locals of the (ddmodify) function.

(defun ddmodify_init ()
  ;;
  ;; These three functions modify the enitity list for common properties.  Since
  ;; color, ltype, and thickness are absent from the object list when they are
  ;; set to their defaults (i.e. color = bylayer), a simple substitution using
  ;; SUBST is not possible.
  ;;
  (defun modify_properties ()
    (emod ecolor 62)
    (emod eltype 6)
    (emod ethickness 39)
    (emod eltscale 48)
    (tempmod elayer 8 nil)
  )
  ;;
  ;; This function is used for modifying common properties of the ACIS
  ;; based geometry objects. Thickness is not valid for these objects
  ;; so it is left out.
  ;;
  (defun modify_prop_geom ()
    (emod ecolor 62)
    (emod eltype 6)
    (emod eltscale 48)
    (tempmod elayer 8 nil)
  )

  (defun emod (value bit)
    (if (= bit 62)
      (progn
        (if (or (= value "BYLAYER")
                (= value "BYLAYER")) (setq value 256))
        (if (or (= value "BYBLOCK")
                (= value "BYBLOCK")) (setq value 0))
      )
    )
    (if (setq oldlist (cdr (assoc bit elist)))
      (tempmod value bit nil)
      (setq elist (append elist (list (cons bit value))))
    )
  )
  ;;
  ;; Resets object list to original values.  Called when the dialogue or
  ;; function is cancelled.
  ;;
  (defun reset ()
    (setq elist  old-elist
          ecolor (cdr (assoc 62 old-elist))
          ethickness (cdr (assoc 39 old-elist))
          eltype (cdr (assoc 6 old-elist))
          elayer (cdr (assoc 8 old-elist))
          eltscale (cdr (assoc 48 old-elist))
    )
    (if (not ecolor) (setq ecolor "BYLAYER"))
    (if (not eltype) (setq eltype "BYLAYER"))
    (if (not ethickness) (setq ethickness 0))
    (if (not eltscale) (setq eltscale 1))
    (modify_properties)
    (setq reset_flag t)
    (entmod elist)
  )
  ;;
  ;; Modify object when dialogue is temporarily dismissed to reflect latest
  ;; settings of dialogue.  It converts the point from current UCS coordinates to
  ;; the proper object coordinates (world or object).
  ;;
  ;; Arguments: value - in current UCS coordinates
  ;;            bit   - object code (i.e. 10 for start point)
  ;;            ptype - point type  0=world 1=planar
  ;;
  (defun tempmod (value bit ptype / newpoint)
    (cond
      ((= ptype 1) (setq value (trans value 1 ename)))
      ((= ptype 0) (setq value (trans value 1 0)))
    )
    (setq elist (subst (cons bit value)
                       (assoc bit elist)
                       elist
                )
    )
  )
  ;;
  ;; The following functions are called after a dialogue has been temporarily
  ;; dismissed and the user is selecting a point.  If a point is selected the
  ;; object list is modified and new X,Y,Z values set.  If no point is selected
  ;; (null response), then the point is reset back to its previous values.
  ;;
  (defun ver_pt1 (ptype)
    (if pt1
      (progn
        (tempmod pt1 10 ptype)
        (entmod elist)
      )
      (setq pt1 (list x1 y1 z1))
    )
  )

  ; (move_pt1 <ptype> )
  ;
  ; Called in liew of (ver_pt1) to translate block insertions which
  ; might have variable attributes attached to them.  If the distance
  ; the block is to be moved is < 1e-6, the move is deferred.

  (defun move_pt1 (ptype / basept hi)
    (setq basept (trans (cdr (assoc 10 (entget ename))) ename 1))
    (cond
       (  (not pt1)
          (setq pt1 (list x1 y1 z1)))

       (  (> 1e-6 (distance pt1 basept)))

       (t (tempmod pt1 10 ptype)
          (setq hi (getvar "highlight"))
          (setvar "highlight" 0)
          (command "._move" ename "" basept pt1)
          (setvar "highlight" hi))
    )
  )

  (defun ver_pt2 (ptype)
    (if pt2
      (progn
        (tempmod pt2 11 ptype)
        (entmod elist)
      )
      (setq pt2 (list x2 y2 z2))
    )
  )

  (defun ver_pt3 (ptype)
    (if pt3
      (progn
        (tempmod pt3 12 ptype)
        (entmod elist)
      )
      (setq pt3 (list x3 y3 z3))
    )
  )

  (defun ver_pt4 (ptype)
    (if pt4
      (progn
        (tempmod pt4 13 ptype)
        (entmod elist)
      )
      (setq pt4 (list x4 y4 z4))
    )
  )
  ;; Xline/Ray
  (defun ver_xline_pt1()
    (if xline_pt1
      (progn
        ;; convert to WCS.
        (setq value (trans xline_pt1 1 0))
        (setq elist (subst (cons 10 value)
                          (assoc 10 elist)
                           elist
                    )
        )
      )
      (setq xline_pt1 (list xline_x1 xline_y1 xline_z1))
    )
  )

  (defun ver_xline_pt2()
    (if xline_pt2
      (progn
    ;;
    ;; Calculate new Direction Vector WCS
    ;; x / (sqrt ( (x**2) + (y**2) + (z**2)     ))
    (setq temp_dir (trans xline_pt2 1 0))
    (setq temp_dir_x (car temp_dir))
    (setq temp_dir_y (cadr temp_dir))
    (setq temp_dir_z (caddr temp_dir))

    (setq temp_xline_pt1 (trans xline_pt1 1 0))
    (setq temp_xline_x1 (car temp_xline_pt1))
    (setq temp_xline_y1 (cadr temp_xline_pt1))
    (setq temp_xline_z1 (caddr temp_xline_pt1))

    (setq denom (sqrt (+ (expt (- temp_dir_x temp_xline_x1) 2)
                         (expt (- temp_dir_y temp_xline_y1) 2)
                         (expt (- temp_dir_z temp_xline_z1) 2)
    )))
    (setq temp_dir_x (/ (- temp_dir_x temp_xline_x1) denom))
    (setq temp_dir_y (/ (- temp_dir_y temp_xline_y1) denom))
    (setq temp_dir_z (/ (- temp_dir_z temp_xline_z1) denom))
    (setq elist (subst (cons 11 (list temp_dir_x temp_dir_y temp_dir_z))
                       (assoc 11 elist)
                       elist
                )
    )
    (entmod elist)
        (setq xline_x2 (car xline_pt2))
        (setq xline_y2 (cadr xline_pt2))
        (setq xline_z2 (caddr xline_pt2))


      )
      (setq xline_pt2 (list xline_x2 xline_y2 xline_z2))
    )
  )

  ;;
  ;; Common properties for all objects
  ;;
  (defun set_tile_props ()
    (set_tile "error" "")
    (setcolor)
    (cond
      ((= eltype "BYLAYER")
         (set_tile "t_ltype" (bylayer_lt)))
      ((= eltype "BYBLOCK")
         (set_tile "t_ltype" "BYBLOCK"))
      (T (set_tile "t_ltype" eltype))
    )
    (set_tile "t_layer" elayer)
    (set_tile "eb_thickness" (ai_rtos ethickness))
    (set_tile "eb_ltscale" (ai_rtos eltscale))
    (setq which_tiles (ai_common_state etype))
    ;; Disable tiles if need be...

    ;; Layer Button and Text Field
    (if (/= 1 (logand 1 which_tiles))
      (progn
        (mode_tile "t_layer" 1)
        (mode_tile "b_name" 1)
      )
    )
    ;; Color Button and Text Field
    (if (/= 2 (logand 2 which_tiles))
      (progn
        (mode_tile "t_color" 1)
        (mode_tile "b_color" 1)
        (mode_tile "show_image" 1)
      )
    )
    ;; Linetype Button and Text Field
    (if (/= 4 (logand 4 which_tiles))
      (progn
        (mode_tile "t_ltype" 1)
        (mode_tile "b_line" 1)
      )
    )
    ;; Linetype Scale Edit Field
    (if (/= 8 (logand 8 which_tiles))
      (progn
        (mode_tile "eb_ltscale" 1)
      )
    )
    ;; Thickness Edit Field.
    (if (/= 16 (logand 16 which_tiles))
      (progn
        (mode_tile "eb_thickness" 1)
      )
    )
  )
  ;;
  ;; XYZ Point values for all enitites
  ;;
  (defun set_tile_pt1 (ptype)
    (if (= ptype 0)
      (setq pt1 (trans (cdr (assoc 10 elist)) 0 1))
      (setq pt1 (trans (cdr (assoc 10 elist)) ename 1))
    )
    (set_tile "x1_pt" (ai_rtos (setq x1 (car pt1))))
    (set_tile "y1_pt" (ai_rtos (setq y1 (cadr pt1))))
    (set_tile "z1_pt" (ai_rtos (setq z1 (caddr pt1))))
  )
  (defun set_tile_pt2 (ptype)
    (if (= ptype 0)
      (setq pt2 (trans (cdr (assoc 11 elist)) 0 1))
      (setq pt2 (trans (cdr (assoc 11 elist)) ename 1))
    )
    (set_tile "x2_pt" (ai_rtos (setq x2 (car pt2))))
    (set_tile "y2_pt" (ai_rtos (setq y2 (cadr pt2))))
    (set_tile "z2_pt" (ai_rtos (setq z2 (caddr pt2))))
  )
  (defun set_tile_pt3 (ptype)
    (if (= ptype 0)
      (setq pt3 (trans (cdr (assoc 12 elist)) 0 1))
      (setq pt3 (trans (cdr (assoc 12 elist)) ename 1))
    )
    (set_tile "x3_pt" (ai_rtos (setq x3 (car pt3))))
    (set_tile "y3_pt" (ai_rtos (setq y3 (cadr pt3))))
    (set_tile "z3_pt" (ai_rtos (setq z3 (caddr pt3))))
  )
  (defun set_tile_pt4 (ptype)
    (if (= ptype 0)
      (setq pt4 (trans (cdr (assoc 13 elist)) 0 1))
      (setq pt4 (trans (cdr (assoc 13 elist)) ename 1))
    )
    (set_tile "x4_pt" (ai_rtos (setq x4 (car pt4))))
    (set_tile "y4_pt" (ai_rtos (setq y4 (cadr pt4))))
    (set_tile "z4_pt" (ai_rtos (setq z4 (caddr pt4))))
  )
  ;;
  ;; Xline/Ray
  ;;
  (defun set_tile_xline_pt1 ()

    (setq xline_pt1 (trans (cdr (assoc 10 elist)) 0 1))

    (set_tile "xline_x1" (ai_rtos (setq xline_x1 (car xline_pt1))))
    (set_tile "xline_y1" (ai_rtos (setq xline_y1 (cadr xline_pt1))))
    (set_tile "xline_z1" (ai_rtos (setq xline_z1 (caddr xline_pt1))))
  )
  (defun set_tile_dirv ()
        ;; Convert 11 group to local UCS (displacement)
    (setq dir_pt (trans (cdr (assoc 11 elist)) 0 1 1))

    (set_tile "dir_x" (rtos (setq dir_ptx (car dir_pt))))
    (set_tile "dir_y" (rtos (setq dir_pty (cadr dir_pt))))
    (set_tile "dir_z" (rtos (setq dir_ptz (caddr dir_pt))))
  )
  (defun set_tile_xline_pt2 ()
    (if (not xline_pt2)
      ;; Initial second point is Root Point + Direction Vector
      (setq xline_pt2 (mapcar '+ xline_pt1 dir_pt))
    )
    (set_tile "xline_x2" (ai_rtos (setq xline_x2 (car xline_pt2))))
    (set_tile "xline_y2" (ai_rtos (setq xline_y2 (cadr xline_pt2))))
    (set_tile "xline_z2" (ai_rtos (setq xline_z2 (caddr xline_pt2))))
  )

  ;;
  ;; Handle for all objects
  ;;
  (defun set_tile_handle ()
    (if (setq hand (cdr (assoc 5 elist)))
      (set_tile "Handle" hand)
      (set_tile "Handle" "None")
    )
  )
  ;;
  ;; Radius for ARC and CIRCLE
  ;;
  (defun set_tile_rad ()
    (setq radius (cdr (assoc 40 elist)))
    (set_tile "radius" (ai_rtos radius))
  )
  ;;
  ;; Start angle for ARC
  ;;
  (defun set_tile_stang ()
    (setq st_ang (cdr (assoc 50 elist)))
    (set_tile "st_ang" (ai_angtos st_ang))
  )
  ;;
  ;; End angle for ARC
  ;;
  (defun set_tile_endang ()
    (setq end_ang (cdr (assoc 51 elist)))
    (set_tile "end_ang" (ai_angtos end_ang))
  )
  ;;
  ;; Rotation Angle - Text, Attributes, Block insertions and Shapes
  ;;
  (defun set_tile_rot ()
    (setq rot (cdr (assoc 50 elist)))
    (set_tile "rot" (ai_angtos rot))
  )
  ;;
  ;; Height - Text, Attributes and Shapes
  ;;
  (defun set_tile_hght ()
    (setq hght (cdr (assoc 40 elist)))
    (set_tile "hght" (ai_rtos hght))
  )
  ;;
  ;; Width Factor - Text, Attributes and Shapes
  ;;
  (defun set_tile_wid ()
    (setq wid (cdr (assoc 41 elist)))
    (set_tile "wid" (ai_rtos wid))
  )
  ;;
  ;; Obliquing Angle - Text, Attributes and Shapes
  ;;
  (defun set_tile_obl ()
    (setq obl (cdr (assoc 51 elist)))
    (set_tile "obl" (ai_angtos obl))
  )
  ;;
  ;; Text string
  ;;
  (defun set_tile_text ()
    (setq text (cdr (assoc 1 elist)))
    (set_tile "t_string" text)
  )
  ;;
  ;; Attribute Tag
  ;;
  (defun set_tile_tag ()
    (if (= etype "ATTDEF")
      (progn
        (setq attag (cdr (assoc 2 elist)))
        (set_tile "tag" attag)
      )
    )
  )
  ;;
  ;; Attribute Definition
  ;;
  (defun set_tile_prompt ()
    (if (= etype "ATTDEF")
      (progn
        (setq atprompt (cdr (assoc 3 elist)))
        (set_tile "prompt" atprompt)
      )
    )
  )
  ;;
  ;; Justification setting for Attributes and Text.  Initializes
  ;; popup list box
  ;;
  (defun set_tile_just ()
    (setq ha (cdr (assoc 72 elist)))  ; horizontal alignment
    (setq va (cdr (assoc 73 elist)))  ; vertical alignment
    (setq ha-prev ha)
    (if (= etype "ATTDEF")
        (setq va (cdr (assoc 74 elist)))  ; vertical alignment
    )
    (setq jlist
          (list "Left"        "Center"        "Right"
                "Aligned"     "Middle"        "Fit"
                "Top left"    "Top center"    "Top right"
                "Middle left" "Middle center" "Middle right"
                "Bottom left" "Bottom center" "Bottom right"
          )
    )
    (start_list "popup_just")
    (mapcar 'add_list jlist)
    (end_list)
    (set_just_idx)
    (set_tile "popup_just" (jlist_act just-idx))
  )
  ;;
  ;; Style setting for Attributes and Text.  Reads symbol table for popup list
  ;; box.
  ;;
  (defun set_tile_style (/ sname style-idx tlist)
    (setq tlist (tblnext "STYLE" T)
          slist nil)
    (while tlist
      (setq sname (cdr (assoc 2 tlist)))
      (if (and (/= sname "")
               (/= (logand 16 (cdr (assoc 70 tlist))) 16))
          (setq slist (cons sname slist)))
      (setq tlist (tblnext "STYLE")))
    (if (>= (getvar "maxsort") (length slist))
        (setq slist (acad_strlsort slist)) ; alphabetize style list
      (setq slist (reverse slist)))     ; or reverse it to put it in DB order

    (start_list "style")
    (mapcar 'add_list slist)
    (end_list)
    (setq tstyle (cdr (assoc 7 elist)))
    (setq style-idx (getindex tstyle slist))
    (set_tile "style" (itoa style-idx))
  )
  ;;
  ;; Text and Attribute setting - upside-down, backwards
  ;;
  (defun set_tile_bk-up ()
    (setq bk-up (cdr (assoc 71 elist)))
    (if (= (logand bk-up 2) 2)
      (set_tile "bkwd" (itoa (setq bkwd 1)))
      (set_tile "bkwd" (itoa (setq bkwd 0)))
    )
    (if (= (logand bk-up 4) 4)
      (set_tile "upsd" (itoa (setq upsd 1)))
      (set_tile "upsd" (itoa (setq upsd 0)))
    )
  )
  ;;
  ;; Attribute setting - invisible, constant, verify, preset
  ;;
  (defun set_tile_icvp ()
    (if (not (setq icvp (cdr (assoc 70 elist))))
      (setq icvp 0)
    )
    (if (= (logand icvp 1) 1)
      (set_tile "inv" (itoa (setq inv 1)))
      (set_tile "inv" (itoa (setq inv 0)))
    )
    (if (= (logand icvp 2) 2)
      (set_tile "con" (itoa (setq con 1)))
      (set_tile "con" (itoa (setq con 0)))
    )
    (if (= (logand icvp 4) 4)
      (set_tile "ver" (itoa (setq vfy 1)))
      (set_tile "ver" (itoa (setq vfy 0)))
    )
    (if (= (logand icvp 8) 8)
      (set_tile "pre" (itoa (setq pre 1)))
      (set_tile "pre" (itoa (setq pre 0)))
    )
  )
  ;;
  ;; Scale factors for block insertions
  ;;
  (defun set_tile_scale (/ temp)
    (setq temp (getvar "LUNITS"))
    (setvar "LUNITS" 2)
    (setq xscale (cdr (assoc 41 elist)))
    (set_tile "xscale" (ai_rtos xscale))
    (setq yscale (cdr (assoc 42 elist)))
    (set_tile "yscale" (ai_rtos yscale))
    (setq zscale (cdr (assoc 43 elist)))
    (set_tile "zscale" (ai_rtos zscale))
    (setvar "LUNITS" temp)
  )
  ;;
  ;; Rows and columns for block insertions
  ;;
  (defun set_tile_rc ()
    (setq columns (cdr (assoc 70 elist)))
    (set_tile "columns" (itoa columns ))
    (setq rows (cdr (assoc 71 elist)))
    (set_tile "rows" (itoa rows))
    (setq col-sp (cdr (assoc 44 elist)))
    (set_tile "col_sp" (ai_rtos col-sp))
    (setq row-sp (cdr (assoc 45 elist)))
    (set_tile "row_sp" (ai_rtos row-sp))
	(if (/= hasclip T)
		(mode_tile "xcliponoff" 1)
		(set_tile "xcliponoff" (itoa xcliponoff))
	)
  )
  ;;
  ;; Invisible edges for 3DFACE
  ;;
  (defun set_tile_edges ()
    (setq f-vis (cdr (assoc 70 elist)))
    (if (= (logand f-vis 1) 1)
      (set_tile "edge_1" (setq edge1 "0"))
      (set_tile "edge_1" (setq edge1 "1"))
    )
    (if (= (logand f-vis 2) 2)
      (set_tile "edge_2" (setq edge2 "0"))
      (set_tile "edge_2" (setq edge2 "1"))
    )
    (if (= (logand f-vis 4) 4)
      (set_tile "edge_3" (setq edge3 "0"))
      (set_tile "edge_3" (setq edge3 "1"))
    )
    (if (= (logand f-vis 8) 8)
      (set_tile "edge_4" (setq edge4 "0"))
      (set_tile "edge_4" (setq edge4 "1"))
    )
  )
  ;;
  ;; XYZ Point values for polyline vertex
  ;;
  (defun set_tile_vpt (ptype)
    (if (= (cdr (assoc 0 vlist)) "LWPOLYLINE")
        (progn
            ;; ctr is 1 based, vertices are zero based.
            (setq vpt (cdr (getLwVert (- ctr 1))))
            (set_tile "xtext" (rtos (setq x1 (car vpt))))
            (set_tile "ytext" (rtos (setq y1 (cadr vpt))))
            (set_tile "ztext" (rtos (setq z1 (caddr vpt))))
        )
        (progn
            (if (= ptype 0)
                (setq vpt (trans (cdr (assoc 10 vlist)) 0 1))
                (setq vpt (trans (cdr (assoc 10 vlist)) ename 1))
            )

            (set_tile "xtext" (rtos (setq x1 (car vpt))))
            (set_tile "ytext" (rtos (setq y1 (cadr vpt))))
            (set_tile "ztext" (rtos (setq z1 (caddr vpt))))
        )
    )

  )
  ;; This is the equivalent to doing an entnext on the heavy weight
  ;; polylines. The hard coded '4' below allow stepping over the
  ;; 40 41 and 42 group code information. The while loop finds the
  ;; first vertex which will be used as the starting assoc in the
  ;; call to nth.
  ;;
  (defun getLwVert (tmpctr / count tmp)
    (setq count 0)
    (while (/= (car (nth count vlist)) 10)
        (setq count (+ count 1))
    )
    ;; If the counter reaches the number of vertices,
    ;; reset ctr and tmpctr to zero again.
    (if (= tmpctr (cdr (assoc 90 vlist)))
        (progn
        (setq ctr 0)
        (setq tmpctr 0)
        )
    )
    (setq tmp (nth (+ count (* tmpctr 4)) vlist))
    (setq tmp (append tmp (list(cdr (assoc 38 vlist)))))
    (setq pt1 (trans (cdr tmp) (cdr (assoc -1 vlist)) 1))
    (setq tmp (cons 10 pt1))
    (setq tmp tmp)
  )
  ;;
  ;; Set tiles for Spline properties. If the spline is rational then we
  ;; need to display the weight values of the control points, so set
  ;; flag to 1.
  ;; 1 = rational spline
  ;; 0 = non-rational spline
  ;;
  (defun set_tile_spline_props ()
    (setq rational_spl_flag 0)      ;; initialize rational spline flag
    (set_tile "Degree" (itoa (cdr (assoc 71 elist))))
    (setq bit70 (cdr (assoc 70 elist)))
    (if (= (logand bit70 1) 1)
        (set_tile "SpProp4" "Closed")
        (set_tile "SpProp4" "      ")
    )
    (if (= (logand bit70 2) 2)
        (set_tile "SpProp3" "Periodic")
        (set_tile "SpProp3" "Non-Periodic")
    )
    (if (= (logand bit70 4) 4)
      (progn
        (set_tile "SpProp2" "Rational")
        (setq rational_spl_flag 1)      ;; this is a rational spline
      )
        (set_tile "SpProp2" "Non-Rational")
    )
    (if (= (logand bit70 8) 8)
        (set_tile "SpProp1" "Planar")
        (set_tile "SpProp1" "Non-Planar")
    )
    (if (= (logand bit70 16) 16)
        (set_tile "SpProp5" "Linear")
        (set_tile "SpProp5" "      ")
    )
  )
  ;;
  ;; XYZ Point values for spline points
  ;; Need to account for WCS/UCS
  ;;
  (defun set_tile_cntl_pt ()
    (setq cntl-pt (cdr (assoc 10  elist)))
    (set_tile "xtext" (rtos (setq x1 (car cntl-pt))))
    (set_tile "ytext" (rtos (setq y1 (cadr cntl-pt))))
    (set_tile "ztext" (rtos (setq z1 (caddr cntl-pt))))
    (if (= rational_spl_flag 1)      ;; if rational spline
      (progn                         ;; show weight
        (setq weight (cdr (assoc 41 elist)))
        (set_tile "weight" (rtos weight))
      )                              ;; else
        (mode_tile "weight_text" 1)  ;; disable weight field
    )
  )
  ;;
  ;; XYZ Point values for spline points
  ;; Need to account for WCS/UCS
  ;;
  (defun set_tile_data_pt ()
    (if (not (assoc 11 elist))
      (mode_tile "data_pts" 1)
      (progn
         (setq data-pt (cdr (assoc 11  elist)))
         ;; display points with current precision.
         (set_tile "dxtext" (rtos (setq x1 (car data-pt))))
         (set_tile "dytext" (rtos (setq y1 (cadr data-pt))))
         (set_tile "dztext" (rtos (setq z1 (caddr data-pt))))
      )
    )
  )
  ;;
  ;; Fit curve, fit spline, or smooth spline surface setting
  ;;
  (defun set_tile_fitsmooth ()
    (cond
      ((= (cdr (assoc 0 elist)) "LWPOLYLINE")
        (set_tile "none" "1")
      )
      ((= (logand bit70 4) 4)
       (cond
         ((= bit75 0)
           (set_tile "none" "1")
           (setq spltype 0)
         )
         ((= bit75 5)
           (set_tile "quad" "1")
           (setq spltype 5)
         )
         ((= bit75 6)
           (set_tile "cubic" "1")
           (setq spltype 6)
         )
         ((= bit75 8)
           (set_tile "bezier" "1")
           (setq spltype 8)
         )
       )
      )
      ((= (logand bit70 2) 2)
        (set_tile "fit" "1")
        (setq spltype 1)
      )
      (T (set_tile "none" "1"))
    )
  )
  ;;
  ;; Closed or Open mesh and polyline setting
  ;;
  (defun set_tile_closed ()
      (if (= pltype "3D mesh")
        (progn
          (if (= (logand bit70 32) 32)
            (set_tile "closedn" (setq closedn "1"))
            (set_tile "closedn" (setq closedn "0"))
          )
          (if (= (logand bit70 1) 1)
            (set_tile "closedm" (setq closed "1"))
            (set_tile "closedm" (setq closed "0"))
          )
          (setq old-closedm closedm old-closedn closedn)
        )
      )
      (if (or (= pltype "2D polyline")
              (= pltype "3D polyline")
          )
        (progn
          (if (= (logand bit70 1) 1)
            (set_tile "closed" (setq closed "1"))
            (set_tile "closed" (setq closed "0"))
          )
          (setq old-closed closed)
        )
      )
  )
  ;; Set common action tiles
  ;;
  ;; Defines action to be taken when pressing various widgets.  It is called
  ;; for every object dialogue.  Not all widgets exist for each object dialogue,
  ;; but defining an action for a non-existent widget does no harm.
  (defun set_action_tiles ()
    (action_tile "cancel"       "(dismiss_dialog 0)")
    (action_tile "accept"       "(dismiss_dialog 1)")
    (action_tile "help"         "(help \"\" help_entry)")
    (action_tile "b_color"      "(getcolor)")
    (action_tile "show_image"   "(getcolor)")
    (action_tile "b_name"       "(setq elayer (getlayer))")
    (action_tile "b_line"       "(setq eltype (getltype))")
    (action_tile "eb_thickness" "(getthickness $value)")
    (action_tile "eb_ltscale"   "(getltscale $value)")

    (action_tile "pick_1"       "(dismiss_dialog 3)")
    (action_tile "pick_2"       "(dismiss_dialog 4)")
    (action_tile "pick_3"       "(dismiss_dialog 5)")
    (action_tile "pick_4"       "(dismiss_dialog 6)")
    (action_tile "x1_pt"        "(ver_x1 $value)")
    (action_tile "y1_pt"        "(ver_y1 $value)")
    (action_tile "z1_pt"        "(ver_z1 $value)")
    (action_tile "x2_pt"        "(ver_x2 $value)")
    (action_tile "y2_pt"        "(ver_y2 $value)")
    (action_tile "z2_pt"        "(ver_z2 $value)")
    (action_tile "x3_pt"        "(ver_x3 $value)")
    (action_tile "y3_pt"        "(ver_y3 $value)")
    (action_tile "z3_pt"        "(ver_z3 $value)")
    (action_tile "x4_pt"        "(ver_x4 $value)")
    (action_tile "y4_pt"        "(ver_y4 $value)")
    (action_tile "z4_pt"        "(ver_4 $value)")

    ;; Action tiles for Xline & Ray
    (action_tile "xline_x1" "(ver_xline_x1 $value)")
    (action_tile "xline_y1" "(ver_xline_y1 $value)")
    (action_tile "xline_z1" "(ver_xline_z1 $value)")
    (action_tile "xline_x2" "(ver_xline_x2 $value)")
    (action_tile "xline_y2" "(ver_xline_y2 $value)")
    (action_tile "xline_z2" "(ver_xline_z2 $value)")

    (action_tile "edge_1"       "(setq edge1 $value)")
    (action_tile "edge_2"       "(setq edge2 $value)")
    (action_tile "edge_3"       "(setq edge3 $value)")
    (action_tile "edge_4"       "(setq edge4 $value)")

    (action_tile "radius"       "(ver_rad $value)")
    (action_tile "st_ang"       "(ver_ang1 $value)")
    (action_tile "end_ang"      "(ver_ang2 $value)")
    (action_tile "end_eang"     "(ver_eang $value)")
    (action_tile "minrad"       "(ver_minrad $value)")
    (action_tile "majrad"       "(ver_majrad $value)")

    (action_tile "xscale"       "(ver_xscl $value)")
    (action_tile "yscale"       "(ver_yscl $value)")
    (action_tile "zscale"       "(ver_zscl $value)")
    (action_tile "rot"          "(ver_rot $value)")
    (action_tile "columns"      "(ver_col $value)")
    (action_tile "rows"         "(ver_row $value)")
    (action_tile "col_sp"       "(ver_colsp $value)")
    (action_tile "row_sp"       "(ver_rowsp $value)")

    (action_tile "hght"         "(ver_hght $value)")
    (action_tile "wid"          "(ver_wid $value)")
    (action_tile "obl"          "(ver_obl $value)")
    (action_tile "style"        "(style_act $value)")

    (action_tile "t_string"     "(ddgettext)")
    (action_tile "tag"          "(ver_tag)")
    (action_tile "prompt"       "(ddgetprompt)")
    (action_tile "bkwd"         "(setq bkwd (atoi $value))")
    (action_tile "upsd"         "(setq upsd (atoi $value))")
    (action_tile "inv"          "(setq inv (atoi $value))")
    (action_tile "con"          "(setq con (atoi $value))")
    (action_tile "ver"          "(setq vfy (atoi $value))")
    (action_tile "pre"          "(setq pre (atoi $value))")
    (action_tile "popup_just"   "(jlist_act $value)")

    (action_tile "closed"       "(setq closed $value)")
    (action_tile "ltgen"        "(setq ltgen $value)")
    (action_tile "closedm"      "(setq closedm $value)")
    (action_tile "closedn"      "(setq closedn $value)")
    (action_tile "next_v"       "(next_vertex)")
    (action_tile "xcliponoff"   "(setq xcliponoff $value)")

    (action_tile "next_cntlpt"  "(next_cntl_pt)")
    (action_tile "next_datapt"  "(next_data_pt)")

    (action_tile "none"         "(if (radio_gaga \"none\")(set_uv 0))")
    (action_tile "fit"          "(if (radio_gaga \"fit\")(set_uv 1))")
    (action_tile "quad"         "(if (radio_gaga \"quad\")(set_uv 5))")
    (action_tile "cubic"        "(if (radio_gaga \"cubic\")(set_uv 6))")
    (action_tile "bezier"       "(if (radio_gaga \"bezier\")(set_uv 8))")

    (action_tile "u"            "(ver_u $value)")
    (action_tile "v"            "(ver_v $value)")
  )

  (defun ddgettext()
    (setq text (get_tile "t_string"))
  )

  (defun ddgetprompt()
    (setq atprompt (get_tile "prompt"))
  )

  ;; As OW doesn't support disabling of individual radio buttons within
  ;; clusters, a check must be performed as to the legitimacy of the
  ;; button pushed and reset if necessary.
  (defun radio_gaga (pushed)
    (cond
      ((and (= pltype "3D polyline")
            (or (= pushed "fit")
                (= pushed "bezier")
            )
       )
        (set_tile "none" "1")
        nil
      )
      ((and (= pltype "3D mesh")
            (= "fit" pushed)
       )
        (set_tile "none" "1")
        nil
      )
      ((= pltype "Polyface mesh")
        (set_tile "none" "1")
        nil
      )
      ((and (= pltype "2D polyline")
            (= "bezier" pushed)
       )
        (set_tile "none" "1")
        nil
      )
      (T)
    )
  )
  ;;
  (defun set_uv (type_n)
    (setq spltype type_n)
    (if (= pltype "3D mesh")
      (if (= spltype 0)
        (progn
          (set_tile "u" (itoa (setq u 0)))
          (set_tile "v" (itoa (setq v 0)))
        )
        (progn
          (if (= u 0)
            (set_tile "u" (itoa (setq u (getvar "surfu"))))
          )
          (if (= v 0)
            (set_tile "v" (itoa (setq v (getvar "surfv"))))
          )
        )
      )
    )
  )


  ;;
  ;; Verification functions
  ;;
  ;; Verify distance function.  This takes a new X, Y, or Z coordinate or
  ;; distance value, the tile name, and the previous value as arguments.
  ;; If the distance is valid, it returns the distance and resets the tile.
  ;; Otherwise, it returns the previous value, sets the error tile and keeps
  ;; focus on the tile.  Shifting focus to the tile with invalid value can
  ;; trigger a callback from another tile whose value is valid.  In order
  ;; to keep the error message from being cleared by this secondary callback,
  ;; the variable errchk is set and checked.  The last-tile variable is set
  ;; and checked to ensure the error message is properly cleared when the
  ;; user corrects the value and hits return.
  ;;
  (defun verify_d (tile value old-value / coord valid errmsg)
    (setq valid nil errmsg "Invalid input value.")
    (if (setq coord (distof value))
      (progn
        (cond
          ((or (= tile "radius")
               (= tile "hght")
               (= tile "wid")
               (= tile "majrad")
               (= tile "minrad")
               (= tile "eb_ltscale")
           )
           (if (> coord 0)
             (setq valid T)
             (setq errmsg "Value must be positive and nonzero.")
           )
          )
          ((or (= tile "xscale")
               (= tile "yscale")
               (= tile "zscale")
           )
           (if (/= coord 0)
             (setq valid T)
             (setq errmsg "Value must be nonzero.")
           )
          )
          (T (setq valid T))
        )
      )
      (setq valid nil)
    )
    (if valid
      (progn
        (if (or (= errchk 0) (= tile last-tile))
          (set_tile "error" "")
        )
        (set_tile tile (ai_rtos coord))
        (setq errchk 0)
        (setq last-tile tile)
        coord
      )
      (progn
        (mode_tile tile 2)            ; Move focus to offending field
        (mode_tile tile 3)            ; Select offending text
        (set_tile "error" errmsg)
        (setq errchk 1)
        (setq last-tile tile)
        old-value
      )
    )
  )
  ;; Function for Xline coord edit box checking.
  (defun verify_xline (tile value old-value / coord valid errmsg)
    (setq valid nil errmsg "Invalid input value.")
    (if (setq coord (distof value))
      (setq valid T)
      (setq valid nil)
    )
    (if (and coord
          (and (= (atof (get_tile "xline_x1")) (atof (get_tile "xline_x2")) )
               (= (atof (get_tile "xline_y1")) (atof (get_tile "xline_y2")) )
               (= (atof (get_tile "xline_z1")) (atof (get_tile "xline_z2")) )
          )
        )
       (progn
         (setq errmsg
                  "The Root point and the Second point cannot be equal.")
         (setq valid nil)
       )
    )
    (if valid
      (progn
        (if (or (= errchk 0) (= tile last-tile))
          (set_tile "error" "")
        )
        (set_tile tile (ai_rtos coord))
        (setq errchk 0)
        (setq last-tile tile)
        coord
      )
      (progn
        (mode_tile tile 2)            ; Move focus to offending field
        (mode_tile tile 3)            ; Select offending text
        (set_tile "error" errmsg)
        (setq errchk 1)
        (setq last-tile tile)
        old-value
      )
    )
  )

  ;;
  ;; Verify angle function.  This takes an angle and a tile name as arguments.
  ;; If the angle is valid, it returns the angle and resets the tile.
  ;; Otherwise, it sets the error tile and keeps focus on the tile.
  ;;
  (defun verify_a (tile value old-value / ang valid errmsg oblqmax)
    (setq valid  nil
          errmsg "Illegal input value."
    )
    (if (setq ang (angtof value))
      (cond
        ((= tile "obl")               ; Restrict obliquing angle
         (setq oblqmax (+ (* (/ PI 2.0) (/ 85.0 90.0)) 0.000001))
         (if (or (<= ang oblqmax)
                 (>= ang (- (* 2.0 PI) oblqmax))
             )
           (setq valid T)
           (setq errmsg
                 "Value must be between -85 and +85 degrees.")
         )
        )
        (T (setq valid T))            ; Other angles not restricted
      )
      (setq valid nil)                ; Invalid angle input
    )
    (if valid
      (progn
        (if (or (= errchk 0) (= tile last-tile))
          (set_tile "error" "")
        )
        (set_tile tile (ai_angtos ang))
        (setq errchk 0)
        (setq last-tile tile)
        ang
      )
      (progn
        (mode_tile tile 2)            ; Move focus to offending field
        (mode_tile tile 3)            ; Select offending text
        (setq last-tile tile)
        (setq errchk 1)
        (set_tile "error" errmsg)
        old-value
      )
    )
  )
  ;;
  ;; Verify angle function.  This takes an angle and a tile name as arguments.
  ;; If the angle is valid, it returns the angle and resets the tile.
  ;; Otherwise, it sets the error tile and keeps focus on the tile.
  ;; This function is specifically for the end angle of the ellipse object. If
  ;; the end angle resolves to zero then we want to display it as 360.
  ;;
  (defun verify_ae (tile value old-value / ang tempend)
    (if (setq ang (angtof value))
      (progn
        (if (or (= errchk 0) (= tile last-tile))
          (set_tile "error" "")
        )
        (setq tempend (ai_angtos ang))
        (if (= tempend "0")
           (set_tile tile "360")
           (set_tile tile tempend)
        )
        (setq errchk 0)
        (setq last-tile tile)
        ang
      )
      (progn
        (mode_tile tile 2)            ; Move focus to offending field
        (mode_tile tile 3)            ; Select offending text
        (setq last-tile tile)
        (setq errchk 1)
        (set_tile "error" "Illegal input value.")
        old-value
      )
    )
  )
  ;;
  ;; Verify integer function.  This takes an integer and a tile name as
  ;; arguments.  If the integer is valid, it returns the integer and resets the
  ;; tile.  Otherwise, it sets the error tile and keeps focus on the tile.
  ;;
  (defun verify_i (tile value old-value / int valid errmsg)
    (setq valid nil)
    (setq errmsg "Value must be an integer.")
    (setq int (atoi value))
    (if (setq intchk (distof value))
      (cond
        ((or (= tile "columns") (= tile "rows"))
         (if (and (= int intchk)
                  (>= int 0)
                  (<= int 32767)
             )
           (setq valid T)
           (setq errmsg "Value must be an integer between 0 and 32767.")
         )
        )
        ((and (or (= tile "u") (= tile "v")))
         (if (and (= int intchk)
                  (>= int 0)
                  (< int 201)
             )
           (setq valid T)
           (setq errmsg "Value must be an integer between 0 and 200.")
         )
        )
      )
    )
    (if valid
      (progn
        (if (or (= errchk 0) (= tile last-tile))
            (set_tile "error" "")
        )
        (set_tile tile (itoa int))
        (setq errchk 0)
        (setq last-tile tile)
        int
      )
      (progn
        (mode_tile tile 2)            ; Move focus to offending field
        (mode_tile tile 3)            ; Select offending text
        (set_tile "error" errmsg)
        (setq errchk 1)
        (setq last-tile tile)
        old-value
      )
    )
  )
  ;;
  ;; Functions that verify tile values for integers
  ;;
  (defun ver_col (value)
    (setq columns (verify_i "columns" value columns))
  )
  (defun ver_row (value)
    (setq rows (verify_i "rows" value rows))
  )
  (defun ver_u (value)
    (setq u (verify_i "u" value u))
  )
  (defun ver_v (value)
    (setq v (verify_i "v" value v))
  )
  ;;
  ;; Functions that verify tile values for reals
  ;;
  (defun ver_x1 (value)
    (if (setq x1 (verify_d "x1_pt" value x1)) (calc))
  )
  (defun ver_y1 (value)
    (if (setq y1 (verify_d "y1_pt" value y1)) (calc))
  )
  (defun ver_z1 (value)
    (if (setq z1 (verify_d "z1_pt" value z1)) (calc))
  )
  (defun ver_x2 (value)
    (if (setq x2 (verify_d "x2_pt" value x2)) (calc))
  )
  (defun ver_y2 (value)
    (if (setq y2 (verify_d "y2_pt" value y2)) (calc))
  )
  (defun ver_z2 (value)
    (if (setq z2 (verify_d "z2_pt" value z2)) (calc))
  )
  (defun ver_x3 (value)
    (setq x3 (verify_d "x3_pt" value x3))
  )
  (defun ver_y3 (value)
    (setq y3 (verify_d "y3_pt" value y3))
  )
  (defun ver_z3 (value)
    (setq z3 (verify_d "z3_pt" value z3))
  )
  (defun ver_x4 (value)
    (setq x4 (verify_d "x4_pt" value x4))
  )
  (defun ver_y4 (value)
    (setq y4 (verify_d "y4_pt" value y4))
  )
  (defun ver_4 (value)
    (setq z4 (verify_d "z4_pt" value z4))
  )
  (defun ver_xscl (value)
    (setq xscale (verify_d "xscale" value xscale))
  )
  (defun ver_yscl (value)
    (setq yscale (verify_d "yscale" value yscale))
  )
  (defun ver_zscl (value)
    (setq zscale (verify_d "zscale" value zscale))
  )
  (defun ver_colsp (value)
    (setq col-sp (verify_d "col_sp" value col-sp))
  )
  (defun ver_rowsp (value)
    (setq row-sp (verify_d "row_sp" value row-sp))
  )
  (defun xclip ()
    (setq xclipmode t)
    (if (= xcliponoff "1")
      (command "_.xclip" (cdr(assoc -1 elist)) "" "_on")
      (command "_.xclip" (cdr(assoc -1 elist)) "" "_off")
    )
  )
  (defun ver_rad (value)
    (if (setq radius (verify_d "radius" value radius))
      (calc)
    )
  )
  (defun ver_majrad (value)
    (if (setq majrad (verify_d "majrad" value majrad))
      (ell_calc_newval "majrad")
    )
  )
  (defun ver_minrad (value)
    (if (setq minrad (verify_d "minrad" value minrad))
      (ell_calc_newval "minrad")
    )
  )
  (defun ver_hght (value)
    (setq hght (verify_d "hght" value hght))
  )
  (defun ver_wid (value)
    (setq wid (verify_d "wid" value wid))
  )
  (defun ver_xline_x1 (value / temp)
    (setq temp xline_x1)
    (setq xline_x1 (verify_xline "xline_x1" value xline_x1))
    (if (/= temp xline_x1)
      (progn (modify_xline 0) (set_tile_dirv) )
    )
  )
  (defun ver_xline_y1 (value / temp)
    (setq temp xline_y1)
    (setq xline_y1 (verify_xline "xline_y1" value xline_y1))
    (if (/= temp xline_y1)
      (progn (modify_xline 0) (set_tile_dirv) )
    )
  )
  (defun ver_xline_z1 (value / temp)
    (setq temp xline_z1)
    (setq xline_z1 (verify_xline "xline_z1" value xline_z1))
    (if (/= temp xline_z1)
      (progn (modify_xline 0) (set_tile_dirv) )
    )
  )
  (defun ver_xline_x2 (value / temp)
    (setq temp xline_x2)
    (setq xline_x2 (verify_xline "xline_x2" value xline_x2))
    (if (/= temp xline_x2)
      (progn (modify_xline 0) (set_tile_dirv))
    )
  )
  (defun ver_xline_y2 (value / temp)
    (setq temp xline_y2)
    (setq xline_y2 (verify_xline "xline_y2" value xline_y2))
    (if (/= temp xline_y2)
      (progn (modify_xline 0) (set_tile_dirv) )
    )
  )
  (defun ver_xline_z2 (value / temp)
    (setq temp xline_z2)
    (setq xline_z2 (verify_xline "xline_z2" value xline_z2))
    (if (/= temp xline_z2)
      (progn (modify_xline 0) (set_tile_dirv) )
    )
  )
  ;;
  ;; Functions that verify tile values for angles
  ;;
  (defun ver_ang1 (value)
    (if (setq st_ang (verify_a "st_ang" value st_ang))
      (calc)
    )
  )
  (defun ver_ang2 (value)
    (if (setq end_ang (verify_a "end_ang" value end_ang))
      (calc)
    )
  )
  ;;
  ;; Verify tile value for ellipse end angle. Handled slightly
  ;; differently than the other angles.
  ;;
  (defun ver_eang (value)
    (setq end_eang (verify_ae "end_eang" value end_eang))
  )
  (defun ver_rot (value)
    (setq rot (verify_a "rot" value rot))
  )
  (defun ver_obl (value)
    (setq obl (verify_a "obl" value obl))
  )
  ;;
  ;; Function that verifies attribute tag field for null string,
  ;; or a string that contains one or more spaces.  Tile value
  ;; is also converted to upper-case as well.
  ;;
  ;;
  (defun ver_tag ( / tval)
     (setq tagval (get_tile "tag"))
     (set_tile "error" "")
     (cond
        (  (or (eq "" (setq tval (strcase (ai_strtrim tagval))))
               (wcmatch tval "* *"))
           (set_tile "error" "Invalid attribute tag.")
           (mode_tile "tag" 2)
           (mode_tile "tag" 3))
        (t (set_tile "error" "")
           (set_tile "tag" tval)
           (setq attag tval)))
  )
  ;;
  ;; Calculation functions
  ;;
  (defun calc ()
    (if (= etype "LINE") (line_calc))
    (if (= etype "ARC") (arc_calc))
    (if (= etype "CIRCLE") (cir_calc))
  )
  ;;
  ;; Calculation functions for lines, arcs, and circles
  ;;
  (defun line_calc ()
    (setq stpt  (list x1 y1 z1))
    (setq endpt (list x2 y2 z2))
    (set_tile "delta_x" (rtos (- x2 x1)))
    (set_tile "delta_y" (rtos (- y2 y1)))
    (set_tile "delta_z" (rtos (- z2 z1)))
    (set_tile "l_length" (rtos (distance stpt endpt)))
    (set_tile "l_angle" (angtos (angle stpt endpt)))
  )

  (defun cir_calc ( / area units)
        (setq radtest radius)
        (set_tile "Dia" (rtos (* 2 radius)))
        (set_tile "Circum" (rtos (* 2 pi radius)))
        (setq area  (* pi (* radius radius)))
        (setq units (getvar "LUNITS"))
        (if (or (= units 3) (= units 4))
            (progn
               (setq area (/ area 144.0))
               (set_tile "Area" (strcat (rtos area 2) " square ft"))
            )
            (set_tile "Area" (rtos area))
        )
  )

  (defun arc_calc ()
    (setq totang (- end_ang st_ang))
    (while (< totang 0)
      (setq totang (+ totang (* 2 pi)))
    )
    (while (> totang (* 2 pi))
      (setq totang (- totang (* 2 pi)))
    )
    (set_tile "tot_angle" (angtos totang))
    (setq arclen (* (* 2 pi radius) (/ totang (* 2 pi))))
    (set_tile "arclen" (rtos arclen))
  )
  ;;
  ;; Calculate the major radius, minor radius, major axis direction.
  ;; Get radius ratio. Convert Start, end parameters to start and end
  ;; angles. Save Major Radius value in "old_majrad" in case the user
  ;; chooses to input a new Major Radius value later. It's needed to
  ;; calculate a new Major Axis Vector value.
  ;;
  (defun ell_calc ()
        ;; Get major radius from the major axis vector.
        (setq majaxis (cdr (assoc 11 elist)))
        (setq xx (car majaxis))
        (setq yy (cadr majaxis))
        (setq zz (caddr majaxis))
        (setq majrad (sqrt (+ (* xx xx) (* yy yy) (* zz zz) )))
        (set_tile "majrad" (ai_rtos majrad))
        (setq old_majrad majrad)
        ;; Get radius ratio
        (setq rrat (cdr (assoc 40 elist)))
        (set_tile "rratio" (rtos rrat))
        ;; Calculate minor radius
        (setq minrad (* majrad rrat))
        (set_tile "minrad" (ai_rtos minrad))
        ;; display major axis vector
        (set_tile "Majraddirx" (rtos xx))
        (set_tile "Majraddiry" (rtos yy))
        (set_tile "Majraddirz" (rtos zz))

        ;; Convert start parm to start angle
        (setq stparm (cdr (assoc 41 elist)))
        (setq vecx (cos stparm))
        (setq vecy (sin stparm))
        (setq st_ang (atan (* rrat vecy) vecx))
        (setq tempst_ang (ai_angtos st_ang))
        (set_tile "st_ang" tempst_ang)
        ;; Convert end parm to end angle. If end angle evaluates to 0
        ;; degrees then display it as 360 degrees.
        (setq endparm (cdr (assoc 42 elist)))
        (setq evecx (cos endparm))
        (setq evecy (sin endparm))
        (setq end_eang (atan (* rrat evecy) evecx))
        (setq tempend_eang (ai_angtos end_eang))
        (if (= tempend_eang "0")
            (set_tile "end_eang" "360")
            (set_tile "end_eang" tempend_eang)
        )
        ;; Get area of the ellipse.
        (ell_calc_area)
  )
  ;; Calculate area of ellipse. If it is an arc then
  ;; grey out area display.
  ;;
  (defun ell_calc_area ( / area units)
     (if (and (= tempst_ang "0") (= tempend_eang "0"))
         (progn
             (setq area  (* pi majrad minrad))
             (setq units (getvar "LUNITS"))
             (if (or (= units 3) (= units 4))
                 (progn
                     (setq area (/ area 144.0))
                     (set_tile "Area" (strcat (rtos area 2)" square ft"))
                 )
                 (set_tile "Area" (rtos area))
             )
         )
        (mode_tile "Area_text" 1)
     )
  )
  ;;
  ;; Calculate new values for ELLIPSE
  ;; Minor Radius, Area
  ;;
  (defun ell_calc_newval (ell_tile)
    (if (= ell_tile "majrad")
      (progn
         (setq rrat (/ minrad majrad))
         (set_tile "rratio" (rtos rrat))
         (ell_calc_area)
      )
    )
    (if (= ell_tile "minrad")
      (progn
         (setq rrat (/ minrad majrad))
         (set_tile "rratio" (rtos rrat))
         (ell_calc_area)
      )
    )
  )

  (defun set_dimen_props (/ loop a stl txstyname)

    (setq stname (cdr (assoc 3 elist)) ; get style name
          stl (tblnext "DIMSTYLE" T)
          stlist nil)
    ;; Get all style names and list them in alphabetical order
    (while stl
      (setq sname (cdr (assoc 2 stl)))
      (if (/= (logand 16 (cdr (assoc 70 stl))) 16)
           (setq stlist (cons sname stlist)))
      (setq stl (tblnext "DIMSTYLE")))

    (setq len    (length stlist)
          loop   0
          dimsty stname
    )

    ;; alphabetize style list, depending on maxsort
    (if (>= (getvar "maxsort") len)
        (setq stlist (acad_strlsort stlist))
      (setq stlist (reverse stlist)))

    ; *UNNAMED style (dimsty = nil at this point) is replaced with
    ; the current style.  After R13, we require dimensiosn to have a
    ; dimstyle, so we plug any holes where we find them.
    (if (null dimsty)
      (setq dimsty (getvar "dimstyle"))
    )
    ; Show the styles in combo box
    (start_list "mod_style" 2)
    (while (< loop len)
      (add_list (nth loop stlist))
      (setq loop (1+ loop))
    )

    (end_list)

    ; Hilight the style name of the selected entity
    (while (and (< 0 loop) (/= dimsty (nth loop stlist)))
      (setq loop (1- loop))
    )
    (set_tile "mod_style" (itoa loop))

    ; The following is the list dimvars. It must be sorted in the same
    ; order as the resfub returned from DDIM (ADS module).

    (setq dimtbl (list "DIMSTYLE"  "Style name"  "Standard flag values"
        "dimpost"   "dimapost"    "dimblk"    "dimblk1"    "dimblk2"
        "dimscale"  "dimasz"      "dimexo"    "dimdli"     "dimexe"
        "dimrnd"    "dimdle"      "dimtp"     "dimtm"      "dimtxt"
        "dimcen"    "dimtsz"      "dimaltf"   "dimlfac"    "dimtvp"
        "dimtfac"   "dimgap"      "dimtol"    "dimlim"     "dimtih"
        "dimtoh"    "dimse1"      "dimse2"    "dimtad"     "dimzin"
        "dimalt"    "dimaltd"     "dimtofl"   "dimsah"     "dimtix"
        "dimsoxd"   "dimclrd"     "dimclre"   "dimclrt"    "dimunit"
        "dimdec"    "dimtdec"     "dimaltu"   "dimalttd"
        "dimaunit"  "dimjust"     "dimsd1"    "dimsd2"     "dimtolj"
        "dimtzin"   "dimaltz"     "dimalttz"  "dimfit"     "dimupt"
        "dimtxsty"
    ))
    (progn
        (setq sv_dvlist (ddimen_getostate dimsty)
              txstyname (assoc 340 sv_dvlist)
              sv_dvlist (subst (cons 340 (tblobjname "STYLE" (cdr txstyname)))
                                txstyname sv_dvlist)
              dimlist sv_dvlist
        )
    )
    (if (= dimtype "DDLEADER")
      (mode_tile "mod_format" 1)
    )
    (action_tile "mod_style" "(setq dimsty (ddimen_style))")
    (action_tile "mod_text"  "(done_dialog 4)")
    (action_tile "mod_geom" "(setq dimlist (ddimen_dlg dimtype 11 dimsty dimlist))")
    (action_tile "mod_format" "(setq dimlist (ddimen_dlg dimtype 12 dimsty dimlist))")
    (action_tile "mod_annot" "(setq dimlist (ddimen_dlg dimtype 13 dimsty dimlist))")
    (action_tile "accept" "(done_dialog 1)")
  )
  ;;
  ;; Get dimvars that have been restored.
  ;;
  (defun ddimen_getvars (/ elm dvlist dv i)
    (setq i 3
          dvlist (list (cons 0 "DIMSTYLE")
                  (cons 2 (getvar "dimstyle")) (cons 70 0))
    )
    (while (setq dv (nth i dimtbl))
      (setq elm (getvar dv)
            dvlist (append dvlist (list (cons dv elm)))
            i (1+ i)
      )
    )
    dvlist
  )

  ;;
  ;; Get the original states of dimvars. This is for DIMENSION entities.
  ;;
  (defun ddimen_getostate (dimsty / dvlist elm i dvvars)
    (setq dvlist (tblnext "dimstyle" T) ; Get a skelton list
          dvvars (ddimen_getvars)       ; Get dimvars for that entity
          i 1
    )

    ; Create a list that contains values of the selected entity.

    (while (setq elm (nth i dvlist))
      (setq dvlist (subst (cons (car elm) (cdr (nth i dvvars))) elm dvlist)
            i (1+ i)
      )
    )
    dvlist
  )

  ;;
  ;; Restores dimvars of the selected enity.
  ;;
  (defun ddimen_dimsty_restore(/ dimtbl dimsvcurset en)
    (setq dimtbl (list "DIMSTYLE"  "Style name"  "Standard flag values"
        "dimpost"   "dimapost"    "dimblk"    "dimblk1"    "dimblk2"
        "dimscale"  "dimasz"      "dimexo"    "dimdli"     "dimexe"
        "dimrnd"    "dimdle"      "dimtp"     "dimtm"      "dimtxt"
        "dimcen"    "dimtsz"      "dimaltf"   "dimlfac"    "dimtvp"
        "dimtfac"   "dimgap"      "dimtol"    "dimlim"     "dimtih"
        "dimtoh"    "dimse1"      "dimse2"    "dimtad"     "dimzin"
        "dimalt"    "dimaltd"     "dimtofl"   "dimsah"     "dimtix"
        "dimsoxd"   "dimclrd"     "dimclre"   "dimclrt"    "dimunit"
        "dimdec"    "dimtdec"     "dimaltu"   "dimalttd"
        "dimaunit"  "dimjust"     "dimsd1"    "dimsd2"     "dimtolj"
        "dimtzin"   "dimaltz"     "dimalttz"  "dimfit"     "dimupt"
        "dimtxsty"
    ))
    (setq dimsvcurset (ddimen_getvars))  ; Save current variable settings
    (progn
        (setq en (cdr (assoc -1 elist)))
        (command "_.dimstyle" "" "" en)
    )
    dimsvcurset
  )
  ;;
  ;; Modify Leader
  ;;
  (defun ddleader (/ dtypebit blkname bename sublist a  stname n dimtbl
                   dimsty dimlist dimtext svtext dimovr sv_dvlist stlist
                   dimtype dimsvcurset)
    (setq dimtype "DDLEADER"
          dimsvcurset (ddimen_dimsty_restore)
    )
    (if (not (new_dialog "ddleader" dcl_id)) (exit))
    ;; Set initial tile values
    (set_tile_props)
    (set_dimen_props)
    (set_tile_handle)
    ;; Define action for tiles
    (set_action_tiles)
    (mode_tile "mod_text" 1)

    (action_tile "mod_style" "(setq dimsty (ddimen_style))")
    (action_tile "accept" "(setq leadtype (get_tile \"s-s\"))(setq arrow (get_tile \"arrow\"))(done_dialog 1)")
    ;; Get ARROW and TYPE.
    (if (= 1 (logand (cdr (assoc '71 elist)))) (set_tile "arrow" "1"))
    (if (= 1 (logand (cdr (assoc '72 elist))))
      (set_tile "s-s" "spline")
      (set_tile "s-s" "straight")
    )
    ;; Start the dialogue.
    (setq dialog-state (start_dialog))
    (if (= dialog-state 1)
      (progn
        ;; update the style
        (if (/= dimsty stname)
          (progn
            ; Be sure to add (3 . dimstylename) element. R12 *UNNAMED case
            ; doesn't have it.
            (if (null (assoc 3 elist))
              (setq elist (append elist (list (cons 3 dimsty))))
              ; else just replace it.
              (setq elist (subst (cons 3 dimsty) (assoc 3 elist) elist))
            )
            ; refresh sv_dvlist with new dimstyle.
            (setq sv_dvlist (tblsearch "dimstyle" dimsty))
          )
        )
        (if (not (null dimlist))   ; attempted to change dimvars
          (ddimen_complist sv_dvlist dimlist dimtbl)
        )
        ;; update for ARROW.
        (if (= "1" arrow)
          (setq elist (subst (cons 71 (logior 1 (cdr (assoc 71 elist)))) (assoc 71 elist) elist))
          (setq elist (subst (cons 71 (logand (~ 1) (cdr (assoc 71 elist)))) (assoc 71 elist) elist))
        )
        ;; update the TYPE.
        (if (= "spline" leadtype)
          (setq elist (subst (cons 72 (logior 1 (cdr (assoc 72 elist)))) (assoc 72 elist) elist))
          (setq elist (subst (cons 72 (logand (~ 1) (cdr (assoc 72 elist)))) (assoc 72 elist) elist))
        )
        (modify_prop_geom)
        ;; update the Color
        (setq elist (subst (cons 77 ecolor) (assoc 77 elist) elist))
        (entmod elist)
      )
    )
    (ddimen_setvars dimsvcurset)      ; Prepare to exit
  )

  ;;
  ;; Get dimvars of a dimstyle with overrides.
  ;;

  (defun ddimen_getdimvars (dimsty / dimovr dvlist elm i)

    ;; Get override information for the specified entity.
    ;; Leader/Tolerance cannot be restored by DIMSTYLE command, so we
    ;; must obtain override information through this tedious operation.
    (setq dimovr (car (cdr (assoc -3 (entget (cdr (assoc -1 elist)) (list "*")))))
          dvlist (tblsearch "dimstyle" dimsty)
          i 2
    )

    ; Update the list with overrides if overrides exist.

    (if (and dimovr (= "DSTYLE" (cdr (nth 1 dimovr))))
      (progn
        (while (setq elm (cdr (nth i dimovr)))
          (progn
            (if (or (= elm "{") (= elm "}"))
              (setq i (1+ i))
              (progn
                (if (or (and (< 180 elm)
                             (< elm 190)
                        )
                        (and (< 80 elm)
                             (< elm 90)
                        )
                    )
                  (setq elm (- elm 10))
                )
                (setq i (1+ i)
                      elm (cons elm (cdr (nth i dimovr)))
                      dvlist (subst elm (assoc (car elm) dvlist) dvlist)
                      i (1+ i)
                )
              )
            )
          )
        )
      )
    )
    dvlist
  )

  ;;
  ;; Modify POINT
  ;;
  (defun modify_point ()
    (modify_properties)
    (setq pt1 (list x1 y1 z1))
    (tempmod pt1 10 0)
    (entmod elist)
  )

  (defun ddpoint ()
    (if (not (new_dialog "ddpoint" dcl_id)) (exit))
    ;; Set initial tile values
    (set_tile_props)
    (set_tile_handle)
    (set_tile_pt1 0)
    ;; Define action for tiles
    (set_action_tiles)
    (setq dialog-state (start_dialog))
    (if (= dialog-state 0)
      (reset)
    )
    (if (= dialog-state 3)
      (progn
        (modify_point)
        (setq pt1 (getpoint (list x1 y1 z1)  "\nPoint: "))
        (ver_pt1 0)
        (ddpoint)
      )
    )
    (if (= dialog-state 1)
      (modify_point)
    )
  )
  ;;
  ;; Modify LINE
  ;;
  (defun modify_line ()
    (modify_properties)
    (setq pt1 (list x1 y1 z1))
    (setq pt2 (list x2 y2 z2))
    (tempmod pt1 10 0)
    (tempmod pt2 11 0)
    (entmod elist)
  )
  (defun ddline ()
    (if (not (new_dialog "ddline" dcl_id)) (exit))
    ;; Set initial tile values
    (set_tile_props)
    (set_tile_handle)
    (set_tile_pt1 0)
    (set_tile_pt2 0)
    (line_calc)
    ;; Define action for tiles
    (set_action_tiles)
    (setq dialog-state (start_dialog))
    (if (= dialog-state 0)
      (reset)
    )
    (if (= dialog-state 3)
      (progn
        (modify_line)
        (setq pt1 (getpoint (list x2 y2 z2)  "\nFrom point: "))
        (ver_pt1 0)
        (ddline)
      )
    )
    (if (= dialog-state 4)
      (progn
        (modify_line)
        (setq pt2 (getpoint (list x1 y1 z1) "\nTo point: "))
        (ver_pt2 0)
        (ddline)
      )
    )
    (if (= dialog-state 1)
      (modify_line)
    )
  )
  ;;
  ;; Modify MLine
  ;;
  (defun modify_mline ()
    (modify_properties)
    (entmod elist)
  )
  (defun ddmline ()
    (if (not (new_dialog "ddmline" dcl_id)) (exit))
    ;; Set initial tile values
    (set_tile_props)
    (set_tile_handle)
    ;; Set mline style text field.
    (set_tile "ml_style" (cdr (assoc '2 elist)))

    ;; Define action for tiles
    (set_action_tiles)
    (action_tile "ml_edit" "(done_dialog 3)")
    (setq dialog-state (start_dialog))
    ;; Dialog cancelled, reset to original values.
    (if (= dialog-state 0)
      (reset)
    )
    ;; Dialog OKed, update the mline.
    (if (= dialog-state 1)
      (modify_mline)
    )
    ;; Edit Mline, call MLEDIT.
    (if (= dialog-state 3)
      (progn
        (modify_mline)
        (command "_mledit")
        (ddmline)
      )
    )
  )
  ;;
  ;; Modify Xline
  ;;
  (defun modify_xline (flag)
    (modify_prop_geom)
    (setq xline_pt1 (list xline_x1 xline_y1 xline_z1))
    (setq xline_pt2 (list xline_x2 xline_y2 xline_z2))
    ;; Update the Root point.
    (setq elist (subst (cons 10 (trans xline_pt1 1 0))
                       (assoc 10 elist)
                       elist
                )
    )
    ;;
    ;; Calculate new Direction Vector WCS
    ;; x / (sqrt ( (x**2) + (y**2) + (z**2)     ))

    (setq temp_dir (trans xline_pt2 1 0))
    (setq temp_dir_x (car temp_dir))
    (setq temp_dir_y (cadr temp_dir))
    (setq temp_dir_z (caddr temp_dir))

    (setq temp_xline_pt1 (trans xline_pt1 1 0))
    (setq temp_xline_x1 (car temp_xline_pt1))
    (setq temp_xline_y1 (cadr temp_xline_pt1))
    (setq temp_xline_z1 (caddr temp_xline_pt1))

    (setq denom (sqrt (+ (expt (- temp_dir_x temp_xline_x1) 2)
                         (expt (- temp_dir_y temp_xline_y1) 2)
                         (expt (- temp_dir_z temp_xline_z1) 2)
    )))
    (setq temp_dir_x (/ (- temp_dir_x temp_xline_x1) denom))
    (setq temp_dir_y (/ (- temp_dir_y temp_xline_y1) denom))
    (setq temp_dir_z (/ (- temp_dir_z temp_xline_z1) denom))
    (setq elist (subst (cons 11 (list temp_dir_x temp_dir_y temp_dir_z))
                       (assoc 11 elist)
                       elist
                )
    )
    (if (= 1 flag)
      (entmod elist)
    )
  )

  (defun ddxline ()
    (if (= etype "XLINE")
      (if (not (new_dialog "ddxline" dcl_id)) (exit))
      (if (not (new_dialog "ddray" dcl_id)) (exit))
    )
    ;; Set initial tile values
    (set_tile_props)
    (set_tile_handle)
    (set_tile_xline_pt1)
    ;; Convert to UCS and post Direction vector.
    (set_tile_dirv)
    ;; Calculate second point by adding Root Point + Direction Vector.
    (set_tile_xline_pt2)
    ;; Define action for tiles
    (set_action_tiles)
    (setq dialog-state (start_dialog))
    (if (= dialog-state 0)
      (reset)
    )
    (if (= dialog-state 3)
      (progn
        (modify_xline 1)
        (while (equal xline_pt2
          (setq xline_pt1 (getpoint xline_pt2 "\nRoot point: ")) 0.000001)
          (princ "The Root point cannot equal the Second point.")
        )
        (ver_xline_pt1)
        (ver_xline_pt2)
(princ)
        (ddxline)
      )
    )
    (if (= dialog-state 4)
      (progn
        (modify_xline 1)
        (while (equal xline_pt1
          (setq xline_pt2 (getpoint xline_pt1 "\nSecond point: ")) 0.000001)
          (princ "The Second point cannot equal the Root point.")
        )
        (ver_xline_pt2)
(princ)
        (ddxline)
      )
    )
    (if (= dialog-state 1)
      (modify_xline 1)
    )
  )
  ;;
  ;; Modify ELLIPSE
  ;;
  (defun modify_ellipse ()
    (modify_prop_geom)
    ;; Update Ellipse Center Point value.
    (setq pt1 (list x1 y1 z1))
    (tempmod pt1 10 1)
    ;; Update Start Parameter value.
    (setq y_val (sin st_ang))
    (setq x_val (* rrat (cos st_ang)))
    (setq stparm (atan y_val x_val))
    (setq elist (subst (cons 41 stparm) (assoc 41 elist) elist ))
    ;; Update End Parameter value. Normalize it, if it's less than
    ;; the start parameter.
    (setq y_eval (sin end_eang))
    (setq x_eval (* rrat (cos end_eang)))
    (setq endparm (atan y_eval x_eval))
    (setq diffparm (- endparm stparm))
    ;; Epsilon of 1.0e-6 radians for checking a zero length arc.
    ;; Since zero length arcs are not allowed - construct the full
    ;; ellipse in this case.
    (if (<= (* diffparm diffparm) 1.0e-12)
        (setq endparm (+ stparm (* 2 pi)))
    )
    (if (<= endparm stparm)
        (setq endparm (+ endparm (* 2 pi)))
    )
    (setq elist (subst (cons 42 endparm) (assoc 42 elist) elist ))
    ;; Calculate the Major Axis Vector by first calculating
    ;; a unit vector using the old Major Radius value. Then
    ;; Multiplying that by the (possibly) new Major Radius
    ;; value to get the new Major Axis Vector value.
    (setq unitxx (/ xx old_majrad))
    (setq unityy (/ yy old_majrad))
    (setq unitzz (/ zz old_majrad))
    (setq newvecxx (* unitxx majrad))
    (setq newvecyy (* unityy majrad))
    (setq newveczz (* unitzz majrad))
    (setq newmajaxis (list newvecxx newvecyy newveczz))
    ;; Update Major Axis Vector value
    (setq elist (subst (cons 11 newmajaxis) (assoc 11 elist) elist ))
    ;; Update Radius Ratio value
    (setq elist (subst (cons 40 rrat) (assoc 40 elist) elist ))
    (entmod elist)
  )

  (defun ddellipse ()
    (if (not (new_dialog "ddellipse" dcl_id)) (exit))
    ;; Set initial tile values
    (set_tile_props)
    (set_tile_handle)
    (set_tile_pt1 1)
    (ell_calc)
    ;; Define action for tiles
    (set_action_tiles)
    (setq dialog-state (start_dialog))
    (if (= dialog-state 0)
      (reset)
    )
    (if (= dialog-state 1)
      (modify_ellipse)
    )
    (if (= dialog-state 3)
      (progn
        (modify_ellipse)
        (setq pt1 (getpoint (list x1 y1 z1)  "\nCenter point: "))
        (ver_pt1 1)
        (ddellipse)
      )
    )
  )
  ;;
  ;; Modify REGION
  ;;
  (defun modify_region ()
    (modify_prop_geom)
    (entmod elist)
  )

  (defun ddregion ()
    (if (not (new_dialog "ddregion" dcl_id)) (exit))
    ;; Set initial tile values
    (set_tile_props)
    (set_tile_handle)
    ;; Define action for tiles
    (set_action_tiles)
    (setq dialog-state (start_dialog))
    (if (= dialog-state 0)
      (reset)
    )
    (if (= dialog-state 1)
      (modify_region)
    )
  )
  ;;
  ;; Modify 3DSOLID
  ;;
  (defun modify_3dsolid ()
    (modify_prop_geom)
    (entmod elist)
  )

  (defun dd3dsolid ()
    (if (not (new_dialog "dd3dsolid" dcl_id)) (exit))
    ;; Set initial tile values
    (set_tile_props)
    (set_tile_handle)
    ;; Define action for tiles
    (set_action_tiles)
    (setq dialog-state (start_dialog))
    (if (= dialog-state 0)
      (reset)
    )
    (if (= dialog-state 1)
      (modify_3dsolid)
    )
  )
  ;;
  ;; Modify AcDbHatch
  ;;
  (defun modify_hatch ()
    (modify_prop_geom)
    (entmod elist)
  )

  (defun ddnewhatch ()
    (if (equal hatch-elist nil)
         (setq hatch-elist old-elist)
    )

    (if (not (new_dialog "ddnewhatch" dcl_id)) (exit))

    ;; disable the thickness tile
    (mode_tile "eb_thickness" 1)
    (mode_tile "e_thickness" 1)

    (setq help_entry "modify_associative_hatch_dialog")
    (set_tile_props)
    (set_tile_handle)

    ;; Define action for tiles
    (set_action_tiles)
    (action_tile "b_hatch" "(done_dialog 2)")
    (setq dialog-state (start_dialog))
    (cond
       (  (eq dialog-state 0)
          (setq old-elist hatch-elist)
          (setq hatch-elist nil)
          (if (= (checkforlockedlayer ename) nil)
             (reset)
             (progn         ;;; special handling for locked layer reset
                ;; unlock the layer
                (setq layername (cdr (assoc 8 (cdr (entget ename)))))
                (command "_.-LAYER" "_Unlock" layername "")
                ;; reset modifiction
                (reset)
                ;; lock the layer again
                (command "_.-LAYER" "_Lock" layername "")
            )
          )
          nil       ;;; makes (ddnewhatch) return nil for Cancel
       )
       (  (eq dialog-state 1)
          (setq hatch-elist nil)
          (modify_hatch)
          T         ;;; makes (ddnewhatch) return T for Ok
       )
       (  (eq dialog-state 2)

          (if (= (cdr (assoc 70 (tblsearch "LAYER" elayer)))
                 4
              )             
              ;;; new selected layer is on a locked layer
              ;;; we can simply modify the hatch properties
              (modify_hatch)
              (progn
                    (if (= (cdr (assoc 70 (tblsearch "LAYER" (cdr (assoc 8 (cdr elist))))))
                            4
                        )           
                        ;;; We changed the layer previously and now the hatch lies on a locked layer.
                        ;;; We have to unlock the layer forethat we can update the hatch properties.
                        ;;; If we don't do that (entmod) fails to update to the new layer.
                        (progn
                            (setq layername (cdr (assoc 8 (cdr elist))))
                            (command "_.-LAYER" "_Unlock" layername "")     ;;; unlock the layer
                            (modify_hatch)                                  ;;; update the properties
                            (command "_.-LAYER" "_Lock" layername "")       ;;; lock the layer again
                        )
                        ;;;  All other cases we simply update the properties
                        (modify_hatch)
                    )
              )
          ) 
          (if (= (checkforlockedlayer ename) nil)
              (command "_hatchedit" ename)
              (alert ;|MSG111|;"The hatch object is on a locked layer.")
          )
          (ddmodify ename)
       )
    )
  )
  ;;
  ;; Modify BODY
  ;;
  (defun modify_body ()
    (modify_prop_geom)
    (entmod elist)
  )

  (defun ddbody ()
    (if (not (new_dialog "ddbody" dcl_id)) (exit))
    ;; Set initial tile values
    (set_tile_props)
    (set_tile_handle)
    ;; Define action for tiles
    (set_action_tiles)
    (setq dialog-state (start_dialog))
    (if (= dialog-state 0)
      (reset)
    )
    (if (= dialog-state 1)
      (modify_body)
    )
  )
  ;;
  ;; Modify CIRCLE
  ;;
  (defun modify_circle ()
    (modify_properties)
    (setq pt1 (list x1 y1 z1))
    (tempmod pt1 10 1)
    (tempmod radius 40 nil)
    (entmod elist)
  )

  (defun ddcircle ()
    (if (not (new_dialog "ddcircle" dcl_id)) (exit))
    ;; Set initial tile values
    (set_tile_props)
    (set_tile_handle)
    (set_tile_pt1 1)
    (set_tile_rad)
    (cir_calc)
    ;; Define action for tiles
    (set_action_tiles)
    (set_tile_pt1 1)
    (setq dialog-state (start_dialog))
    (if (= dialog-state 0)
      (reset)
    )
    (if (= dialog-state 1)
      (modify_circle)
    )
    (if (= dialog-state 3)
      (progn
        (modify_circle)
        (setq pt1 (getpoint (list x1 y1 z1)  "\nCenter point: "))
        (ver_pt1 1)
        (ddcircle)
      )
    )
  )
  ;;
  ;; Modify ARC
  ;;
  (defun modify_arc ()
    (modify_properties)
    (setq pt1 (list x1 y1 z1))
    (tempmod pt1 10 1)
    (tempmod radius 40 nil)
    (tempmod st_ang 50 nil)
    (tempmod end_ang 51 nil)
    (entmod elist)
  )
  (defun ddarc ()
    (if (not (new_dialog "ddarc" dcl_id)) (exit))
    ;; Set initial tile values
    (set_tile_props)
    (set_tile_handle)
    (set_tile_pt1 1)
    (set_tile_rad)
    (set_tile_stang)
    (set_tile_endang)
    (arc_calc)
    ;; Define action for tiles
    (set_action_tiles)
    (setq dialog-state (start_dialog))
    (if (= dialog-state 0)
      (reset)
    )
    (if (= dialog-state 1)
      (modify_arc)
    )
    (if (= dialog-state 3)
      (progn
        (modify_arc)
        (setq pt1 (getpoint (list x1 y1 z1) "\nCenter point: "))
        (ver_pt1 1)
        (ddarc)
      )
    )
  )
  ;;
  ;; Modify SOLID or TRACE
  ;; Note the Z value of the object is determined by the Z value of the fourth
  ;; point - code 13.  Changing the point values of a solid or trace from a UCS
  ;; that is nonplanar to the UCS the object was created may confuse the user.
  (defun modify_solid ()
    (modify_properties)
    (setq pt1 (list x1 y1 z4))
    (setq pt2 (list x2 y2 z4))
    (setq pt3 (list x3 y3 z4))
    (setq pt4 (list x4 y4 z4))
    (tempmod pt1 10 1)
    (tempmod pt2 11 1)
    (tempmod pt3 12 1)
    (tempmod pt4 13 1)
    (entmod elist)
  )

  (defun ddsolid ()
    (if (= etype "SOLID")
        (if (not (new_dialog "ddsolid" dcl_id)) (exit))
        (if (not (new_dialog "ddtrace" dcl_id)) (exit))
    )
    ;; Set initial tile values
    (set_tile_props)
    (set_tile_handle)
    (set_tile_pt1 1)
    (set_tile_pt2 1)
    (set_tile_pt3 1)
    (set_tile_pt4 1)
    ;; Define action for tiles
    (set_action_tiles)
    (setq dialog-state (start_dialog))
    (if (= dialog-state 0)
      (reset)
    )
    (if (= dialog-state 1)
      (modify_solid)
    )
    (if (= dialog-state 3)
      (progn
        (modify_solid)
        (setq pt1 (getpoint (list x1 y1 z1) "\nFirst point: "))
        (ver_pt1 1)
        (ddsolid)
      )
    )
    (if (= dialog-state 4)
      (progn
        (modify_solid)
        (entmod elist)
        (setq pt2 (getpoint (list x2 y2 z2) "\nSecond point: "))
        (ver_pt2 1)
        (ddsolid)
      )
    )
    (if (= dialog-state 5)
      (progn
        (modify_solid)
        (setq pt3 (getpoint (list x3 y3 z3) "\nThird point: "))
        (ver_pt3 1)
        (ddsolid)
      )
    )
    (if (= dialog-state 6)
      (progn
        (modify_solid)
        (setq pt4 (getpoint (list x4 y4 z4) "\nFourth point: "))
        (ver_pt4 1)
        (ddsolid)
      )
    )
  )
  ;;
  ;; Modify 3DFACE
  ;;
  ;; Check visibility of edges
  ;;
  (defun edgetest (/ bit1 bit2 bit3 bit4)
    (if (= edge1 "1") (setq bit1 0) (setq bit1 1))
    (if (= edge2 "1") (setq bit2 0) (setq bit2 2))
    (if (= edge3 "1") (setq bit3 0) (setq bit3 4))
    (if (= edge4 "1") (setq bit4 0) (setq bit4 8))
    (+ bit1 bit2 bit3 bit4)
  )

  (defun modify_3dface ()
    (modify_properties)
    (setq pt1 (list x1 y1 z1))
    (setq pt2 (list x2 y2 z2))
    (setq pt3 (list x3 y3 z3))
    (setq pt4 (list x4 y4 z4))
    (tempmod pt1 10 0)
    (tempmod pt2 11 0)
    (tempmod pt3 12 0)
    (tempmod pt4 13 0)
    (tempmod (edgetest) 70 nil)
    (entmod elist)
  )

  (defun dd3dface ()
    (if (not (new_dialog "dd3dface" dcl_id)) (exit))
    (set_tile_props)
    (set_tile_handle)
    (set_tile_pt1 0)
    (set_tile_pt2 0)
    (set_tile_pt3 0)
    (set_tile_pt4 0)
    (set_tile_edges)
    ;; Define action for tiles
    (set_action_tiles)
    (setq dialog-state (start_dialog))
    (if (= dialog-state 0)
      (reset)
    )
    (if (= dialog-state 1)
      (modify_3dface)
    )
    (if (= dialog-state 3)
      (progn
        (modify_3dface)
        (setq pt1 (getpoint (list x1 y1 z1) "\nFirst pt: "))
        (ver_pt1 0)
        (dd3dface)
      )
    )
    (if (= dialog-state 4)
      (progn
        (modify_3dface)
        (setq pt2 (getpoint (list x2 y2 z2) "\nSecond point: "))
        (ver_pt2 0)
        (dd3dface)
      )
    )
    (if (= dialog-state 5)
      (progn
        (modify_3dface)
        (setq pt3 (getpoint (list x3 y3 z3) "\nThird point: "))
        (ver_pt3 0)
        (dd3dface)
      )
    )
    (if (= dialog-state 6)
      (progn
        (modify_3dface)
        (setq pt4 (getpoint (list x4 y4 z4) "\nFourth point: "))
        (ver_pt4 0)
        (dd3dface)
      )
    )
  )

  ;;
  ;; Image functions
  ;;
  (defun image_scale (/ upixel en n userscale temp)
    ;; Calculate the size of an image pixel in AutoCAD units
    (setq upixel (distance '(0 0 0) (cdr (assoc 11 elist))))

    ;; Retrieve the user scale
    (setq en (dictsearch (namedobjdict) "ACAD_IMAGE_VARS"))

    ;; Next, extract the image units and pixel resolution
    (if (or (= 0 (cdr (assoc 281 image:olist))) (= 0 (cdr (assoc 72 en))))
      (setq image:scale (* (cadr (assoc 13 elist)) upixel))
      (progn
        ;; Convert the user scale to the proper word
        (setq n (cdr (assoc 72 en)))
        (setq temp (getvar "LUNITS"))
        (if (and (/= n 0) (or (= temp 3) (= temp 4))) (setq n 5))
        (if (= n 1) (setq userscale "Millimeter"))
        (if (= n 2) (setq userscale "Centimeter"))
        (if (= n 3) (setq userscale "Meter"))
        (if (= n 4) (setq userscale "Kilometer"))
        (if (= n 5) (setq userscale "Inch"))
        (if (= n 6) (setq userscale "Foot"))
        (if (= n 7) (setq userscale "Yard"))
        (if (= n 8) (setq userscale "Mile"))
        (setq image:scale (/ upixel (cvunit 1 "Millimeter" userscale) (cadr (assoc 11 image:olist))))
      )
    )
  )

  ;;
  ;; Scale factors for block insertions
  ;;
  (defun image_set_tile_scale (/ temp)
    (setq temp (getvar "LUNITS"))
    (setvar "LUNITS" 2)
    (set_tile "xscale" (ai_rtos (* image:scale image:mult)))
    (set_tile "wid" (ai_rtos (* image:width image:mult)))
    (set_tile "hght" (ai_rtos (* image:height image:mult)))
    (setvar "LUNITS" temp)
  )

  ;;
  ;; Set the scale, width and height values appropriately.
  ;;
  (defun image_update (field value / orig_value new_value mult)
    (if (= field "xscale")
        (setq orig_value image:scale)
    )
    (if (= field "wid")
        (setq orig_value image:width)
    )
    (if (= field "hght")
        (setq orig_value image:height)
    )
    (setq new_value (verify_d field value (* image:mult orig_value)))
    (if (/= new_value old_value)
      ;; Make sure the user has entered a sufficiently large value
      (if new_value
        (progn
          (if (< new_value 1e-8)
            (setq mult image:mult)
            (setq mult (/ new_value orig_value))
          )
          (setq image:mult mult)
          (image_set_tile_scale)
        )
      )
    )
  )

  (defun image_disp_opt (bit)
    (setq image:options (Boole 6 image:options bit))
  )

  (defun image_modify ()
    (modify_properties)
    (setq pt1 (list x1 y1 z1))
    (tempmod pt1 10 0)
    (entmod elist)
  )

  (defun image_clean_variables ()
    ;; Clean up global variables used here
    (setq image:scale nil)
    (setq image:angle nil)
    (setq image:width nil)
    (setq image:height nil)
    (setq image:options nil)
    (setq image:oname nil)
    (setq image:olist nil)
    (setq image:mult nil)
    (setq st_ang nil)
  )

  (defun image_scale_vector (v1 value)
    (list (* (car v1) value) (* (cadr v1) value) (* (last v1) value))
  )

  (defun image_cross_product (v1 v2 / vx vy vz)
    (setq vx (- (* (cadr v1) (last v2)) (* (cadr v2) (last v1))))
    (setq vy (- (* (car v2) (last v1)) (* (car v1) (last v2))))
    (setq vz (- (* (car v1) (cadr v2)) (* (car v2) (cadr v1))))
    (list vx vy vz)
  )

  (defun image_dot_product (v1 v2)
    (+ (* (car v1) (car v2)) (* (cadr v1) (cadr v2)) (* (last v1) (last v2)))
  )

  (defun image_add_vector (v1 v2)
    (list (+ (car v1) (car v2)) (+ (cadr v1) (cadr v2)) (+ (last v1) (last v2)))
  )

  (defun image_subtract_vector (v1 v2)
    (list (- (car v1) (car v2)) (- (cadr v1) (cadr v2)) (- (last v1) (last v2)))
  )

  (defun image_normalize_vector (v1)
    (image_scale_vector v1 (/ 1. (sqrt (image_dot_product v1 v1))))
  )

  (defun image_rotate_vector (vector angle axis / along_axis axis_x_in_rot_plane axis_y_in_rot_plane in_rot_plane)
    ;; Normalize the axis
    (setq axis (image_normalize_vector axis))
    (setq along_axis (image_scale_vector axis (image_dot_product vector axis)))
    (setq axis_x_in_rot_plane (image_subtract_vector vector along_axis))
    (setq axis_y_in_rot_plane (image_cross_product axis axis_x_in_rot_plane))
    (setq in_rot_plane
      (image_add_vector
        (image_scale_vector axis_x_in_rot_plane (cos angle))
        (image_scale_vector axis_y_in_rot_plane (sin angle))
      )
    )
    (image_add_vector along_axis in_rot_plane)
  )

  ;;
  ;; Calculate current rotation angle if appropriate.
  ;; If not, grey out the rotation field.
  ;;
  (defun image_rotation (/ normal rlength zlength sqrt_tolerance)
    ;; Determine if image normal is codirectional with
    ;; the current UCS Z-vector.
    ;;
    (setq normal (trans (image_normalize_vector (image_cross_product (cdr (assoc 11 elist)) (cdr (assoc 12 elist)))) 0 1 1))
    (setq rlength (+ (* (car normal) (car normal)) (* (cadr normal) (cadr normal))))
    (setq zlength (* (last normal) (last normal)))
    ;; Note that the value .0001 in the radians is approximately 0.0057 degrees.
    ;;
    ;;                cos(.0001)^2
    ;;   Tolerance = --------------
    ;;                sin(.0001)^2
    ;;
    (setq sqrt_tolerance (/ (cos 0.0001) (sin 0.0001)))
    (if (and (< (* rlength sqrt_tolerance sqrt_tolerance) zlength) (> (last normal) 0))
      (setq image:angle (angle '(0 0 0) (trans (cdr (assoc 11 elist)) 0 1 1)))
      (progn
        (mode_tile "st_ang" 1)
        (setq image:angle 0)
      )
    )
  )

  ;;
  ;; Modify Image
  ;;
  (defun modify_image (/ u_vector v_vector z_vector)
    (modify_properties)
    (setq pt1 (list x1 y1 z1))
    (tempmod pt1 10 0)
    ;; Make display option changes to image
    (emod image:options 70)
    ;; Get the u and v vectors
    (setq u_vector (cdr (assoc 11 elist)))
    (setq v_vector (cdr (assoc 12 elist)))

    ;; Make scale changes to the vectors
    (if (/= image:mult 1)
      (progn
        (setq u_vector (image_scale_vector u_vector image:mult))
        (setq v_vector (image_scale_vector v_vector image:mult))
      )
    )
    ;; Rotate the vectors
    (setq st_ang (- st_ang image:angle))
    (if (/= st_ang 0)
      (progn
        (setq z_vector
          (image_cross_product
            (image_normalize_vector u_vector)
            (image_normalize_vector v_vector)
          )
        )
        (setq u_vector (image_rotate_vector u_vector st_ang z_vector))
        (setq v_vector (image_rotate_vector v_vector st_ang z_vector))
      )
    )
    (emod u_vector 11)
    (emod v_vector 12)
    (entmod elist)
  )

  ;;
  ;; Run imageadjust on given entity, working around possible
  ;; re-entrancy problems
  ;;
  (defun image_adjust (ename)
    (verify_arxapp_loaded "ism.arx") ;; make sure it's loaded...
    (imageadjust ename)
  )

  (defun ddimage ()
    (if (not (new_dialog "ddimage" dcl_id)) (exit))

    ;; Grey out the thickness field.
    (mode_tile "b_thickness" 1)
    (mode_tile "eb_thickness" 1)

    ;; Get the associated def object.
    (setq image:oname (cdr (assoc 340 elist)))
    (setq image:olist (entget image:oname))

    ;; Set initial tile values
    (set_tile_props)
    (set_tile_handle)
    (set_tile_pt1 0)
    (setq image:scale (image_scale))
    (setq st_ang (image_rotation))
    (setq image:width (* (distance '(0 0 0) (cdr (assoc 11 elist))) (cadr (assoc 13 elist))))
    (setq image:height (* (distance '(0 0 0) (cdr (assoc 12 elist))) (caddr (assoc 13 elist))))
    (setq image:options (cdr (assoc 70 elist)))

    ;; Record the last multiplier for use in image_update function
    (setq image:mult 1)

    ;; Retrieve the image name
    (setq fn (reverse (dictsearch (namedobjdict) "ACAD_IMAGE_DICT")))
    (setq flag 0)
    (foreach n fn
      (if (= flag 1)
        (progn
          ;; Display the image name
          (set_tile "image_name" (cdr n))
          (setq flag 2)
        )
      )
      (if (= flag 0)
        (if (equal (cdr n) image:oname) (setq flag 1))
      )
    )

    (set_tile "image_path" (cdr (assoc 1 image:olist)))
    (set_tile "st_ang" (ai_angtos image:angle))
    (image_set_tile_scale)

    ;; Check the appropriate boxes
    (if (/= (Boole 1 image:options 1) 0) (set_tile "image_show" "1"))
    (if (/= (Boole 1 image:options 2) 0) (set_tile "image_non_ortho" "1"))
    (if (/= (Boole 1 image:options 4) 0) (set_tile "image_clipped" "1"))
    (if (/= (Boole 1 image:options 8) 0) (set_tile "image_transparency" "1"))

    ;; Define action for tiles
    (set_action_tiles)
    (action_tile "xscale" "(image_update \"xscale\" $value)")
    (action_tile "st_ang" "(ver_ang1 $value)")
    (action_tile "wid" "(image_update \"wid\" $value)")
    (action_tile "hght" "(image_update \"hght\" $value)")
    (action_tile "image_show" "(image_disp_opt 1)")
    (action_tile "image_non_ortho" "(image_disp_opt 2)")
    (action_tile "image_clipped" "(image_disp_opt 4)")
    (action_tile "image_transparency" "(image_disp_opt 8)")
    (action_tile "image_adjust" "(done_dialog 4)")

    (setq dialog-state (start_dialog))
    (if (= dialog-state 0)
      (progn
        (reset)
        (image_clean_variables)
      )
    )
    (if (= dialog-state 1)
      (progn
        (modify_image)
        (image_clean_variables)
      )
    )
    (if (= dialog-state 3)
      (progn
        (modify_image)
        (image_clean_variables)
        (setq pt1 (getpoint (list x1 y1 z1) "\nNew Insertion Point: "))
        (ver_pt1 0)
        (ddimage)
      )
    )
    (if (= dialog-state 4)
      (progn
        (modify_image)
        (image_adjust ename)
        (setq elist (entget ename))
        (ddimage)
      )
    )
  )

  ;;
  ;; Modify BLOCK (and its Attributes, if any)
  ;;
  (defun modify_block ( / el en2 p1 p2 oldp1 oldp2 new_hgt old_hgt attr
                          new_wid old_wid old_rot scaling old_scale
                          xdelta ydelta zdelta xbase ybase zbase ipos)
    (modify_properties)

    ;; First, translate and scale the Attributes, if there are any.
    (setq old_rot   (cdr (assoc 50 elist))
          old_scale (list (cdr (assoc 41 elist))
                          (cdr (assoc 42 elist))
                          (cdr (assoc 43 elist))
                    )
          scaling   (or (/= xscale (car   old_scale))
                        (/= yscale (cadr  old_scale))
                        (/= zscale (caddr old_scale))
                    )
          attr      nil               ; No Attributes modified yet
    )
    (if scaling
      (progn
        (setq xdelta (/ xscale (car   old_scale))
              ydelta (/ yscale (cadr  old_scale))
              zdelta (/ zscale (caddr old_scale))
              ipos   (cdr (assoc 10 elist))
              xbase  (car   ipos)
              ybase  (cadr  ipos)
              zbase  (caddr ipos)
              en2    (entnext ename)  ; First Attribute
        )

        ; If the Block is rotated, temporarily un-rotate it, along
        ; with all its Attributes, so the scaling/translation of the
        ; Attributes won't have to take the Block rotation into account.
        (if (/= old_rot 0.0)
          (progn
            (command "_rotate" ename "" pt1 (ai_angtos (- old_rot)))
            (setq old_rot 0.0
                  elist   (subst (cons 50 0.0) (assoc 50 elist) elist)
            )
          )
        )

        (while en2
          (setq el (entget en2))
          (if (= (cdr (assoc 0 el)) "ATTRIB")
            (progn
              (setq old_hgt (cdr (assoc 40 el))  ; Height
                    old_wid (cdr (assoc 41 el))  ; Width-factor
                    oldp1 (cdr (assoc 10 el))  ; Generation start point
                    oldp2 (cdr (assoc 11 el))  ; Optional alignment pt
                    ha    (cdr (assoc 72 el))  ; Horizontal alignment
                    va    (cdr (assoc 74 el))  ; Vertical alignment

                    ; Translate gen. start point
                    p1    (list (+ xbase (* xdelta (- (car   oldp1) xbase)))
                                (+ ybase (* ydelta (- (cadr  oldp1) ybase)))
                                (+ zbase (* zdelta (- (caddr oldp1) zbase)))
                          )
                    el    (subst (cons 10 p1) (assoc 10 el) el)
              )

              ; Translate alignment pt similarly, if present and applicable
              (if (and oldp2 (or (/= ha 0)
                                 (/= va 0)
                             )
                  )
                (setq p2 (list (+ xbase (* xdelta (- (car   oldp2) xbase)))
                               (+ ybase (* ydelta (- (cadr  oldp2) ybase)))
                               (+ zbase (* zdelta (- (caddr oldp2) zbase)))
                         )
                      el (subst (cons 11 p2) (assoc 11 el) el)
                )
              )

              ; Each Attribute's height and width-factor were computed
              ; based on the Block's scale factors.  Adjust them now,
              ; by first reducing to values for 1x1 scale...
              (setq new_hgt (/ old_hgt (cadr old_scale))
                    new_wid (/ old_wid (/ (car old_scale) (cadr old_scale)))
              )
              ; ...and then rescaling.
              (setq new_hgt (* new_hgt yscale)
                    new_wid (* new_wid (/ xscale yscale))
              )
              (if (/= new_hgt old_hgt)
                (setq el (subst (cons 40 new_hgt) (assoc 40 el) el))
              )
              (if (/= new_wid old_wid)
                (setq el (subst (cons 41 new_wid) (assoc 41 el) el))
              )
              (entmod el)
              (setq attr T             ; At least one Attribute modified
                    en2  (entnext en2) ; Next Attribute
              )
            )
            (setq en2 nil)          ; No more attributes
          )
        )
      )
    )

    (setq pt1 (list x1 y1 z1))
    (tempmod xscale 41 nil)
    (tempmod yscale 42 nil)
    (tempmod zscale 43 nil)
    (tempmod col-sp 44 nil)
    (tempmod row-sp 45 nil)
    (tempmod columns 70 nil)
    (tempmod rows 71 nil)
    (if (= xclipmode nil)
       (entmod elist)
       (setq xclipmode nil)
    )
    (move_pt1 1)

    ;; Now do the rotation with the ROTATE command.
    (if (/= old_rot rot)
      (command "_rotate" ename "" pt1 (ai_angtos (- rot old_rot)))
      (if attr                          ; Else, if attributes were modified,
        (entupd ename)                  ;   just regen to force attrib display
      )
    )
    (setq elist (entget ename))
  )
  (defun ddblock (/ temp temp_xclip newhatch blkname blklist blktype program xrefpath)
    (setq newhatch 0)
    (setq blkname (cdr (assoc 2 elist)))
    (setq blklist (tblsearch "block" blkname))
    (setq blktype (cdr (assoc 70 blklist)))
    (setq xcliponoff 0)
    (if (or (= blktype 0)(= blktype 36))
            (progn
              (setq xcliponoff (xclipon elist))
          (setq temp_xclip xcliponoff)
        )
    )
    (if (= (logand blktype 4) 4)
      (progn
        (setq xrefpath (cdr (assoc 1 blklist)))
        (setq help_entry "modify_External_Reference_dialog")
        (if (not (new_dialog "ddxref" dcl_id)) (exit))
        (set_tile "Bl_name" blkname)
        (set_tile "path" xrefpath)
      )
      (progn
        ;; Get program name for use as Xdata app name
        (if (not (setq program (getvar "program")))
           (setq program "acad")
        )
        (if (and (setq temp (assoc -3 (entget ename (list program))))
                 (= (cdr (assoc 1000 (cdadr temp))) "HATCH")
                 (assoc 1005 (cdadr temp))
            )
          (progn
              (setq newhatch 1)
              (setq help_entry  "modify_Hatch_dialog")
              ;;; convert the object into a new hatch
              (ai_undo_on) ;; enable undo
              (COMMAND "_.UNDO" "_Mark")
              (COMMAND "_.CONVERT" "_Hatch" "_Select" (cdr (assoc -1 elist)) "")
              ;;; suppress nasty message from convert command
              (princ "\r                                                                      \r")
              (setq elist (entget (setq ename (cdr (assoc -1 elist)))))
              (setq old-elist elist)
              ;;; If we have cancelled ddmodify
              (if (not (ddnewhatch))      ;;; (ddnewhatch) returns nil for Cancel or T for Ok
                   (command "_.UNDO" "_Back")
              )
              (ai_undo_off) ;; restore undo state
          )
          (progn
            (if (not (new_dialog "ddblock" dcl_id)) (exit))
            (if ( = "*" (substr blkname 1 1))
              (set_tile "Bl_name" (strcat blkname " - Anonymous block"))
              (set_tile "Bl_name" blkname)
            )
            (setq help_entry  "modify_Block_Insertion_dialog")
          )
        )
      )
    )
    (if (= newhatch 0)
      (progn
        (set_tile_props)
        (set_tile_handle)
        (set_tile_pt1 1)
        (set_tile_rot)
        (set_tile_scale)
        (set_tile_rc)
        (if (= (logand blktype 1) 1)
          (progn
            (mode_tile "xscale" 1)
            (mode_tile "yscale" 1)
            (mode_tile "zscale" 1)
            (mode_tile "rot" 1)
            (mode_tile "columns" 1)
            (mode_tile "rows" 1)
            (mode_tile "col_sp" 1)
            (mode_tile "row_sp" 1)
          )
        )
        ;; Define action for tiles
        (set_action_tiles)
        (setq dialog-state (start_dialog))
        (cond
           (  (eq dialog-state 0)
              (setq pt1 (trans (cdr (assoc 10 old-elist)) ename 1))
              (move_pt1 1)
              (reset))
           (  (eq dialog-state 1)
              (if (and (/= temp_xclip xcliponoff) (or (= blktype 0)(= blktype 36)))
                (xclip)
              )
              (modify_block))
           (  (eq dialog-state 3)
              (modify_block)
              (setq pt1 (getpoint (list x1 y1 z1)  "\nInsertion point: "))
              (move_pt1 1)
              (ddblock))
           (  (eq dialog-state 4)
              (modify_block)
              ;; Get current handle.
              (setq hand (cdr (assoc 5 elist)))
              (command "_hatchedit" ename)
              ;; If OK in hatchedit, a *new* entity is created and the old one
              ;; is deleted.  So if the old one exists, it must have been a
              ;; so rest the entity.
              (if (entget (handent hand)) (reset))
           )
        )
      )
    )
  )
  ;;
  ;; Modify SHAPE
  ;;
  (defun modify_shape ()
    (modify_properties)
    (setq pt1 (list x1 y1 z1))
    (tempmod pt1 10 1)
    (tempmod hght 40 nil)
    (tempmod wid 41 nil)
    (tempmod rot 50 nil)
    (tempmod obl 51 nil)
    (entmod elist)
  )

  (defun ddshape ()
    (if (not (new_dialog "ddshape" dcl_id)) (exit))
    (set_tile_props)
    (set_tile_handle)
    (set_tile_pt1 1)
    (set_tile_rot)
    (set_tile_hght)
    (set_tile_wid)
    (set_tile_obl)
    (set_tile "sh_name" (cdr (assoc 2 elist)))
    ;; Define action for tiles
    (set_action_tiles)
    (setq dialog-state (start_dialog))
    (if (= dialog-state 0)
      (reset)
    )
    (if (= dialog-state 1)
      (modify_shape)
    )
    (if (= dialog-state 3)
      (progn
        (modify_shape)
        (setq pt1 (getpoint (list x1 y1 z1)  "\nInsertion point: "))
        (ver_pt1 1)
        (ddshape)
      )
    )
  )
  ;;
  ;; Modify TEXT or ATTDEF
  ;;
  ;; Set bit code for upside-down and backwards setting
  ;;
  (defun code_71 ()
    (cond ((and (= bkwd "0") (= upsd "0")) 0)
          ((and (= bkwd "1") (= upsd "0")) 2)
          ((and (= bkwd "0") (= upsd "1")) 4)
          ((and (= bkwd "1") (= upsd "1")) 6)
    )
  )
  ;;
  ;; Style action.  Reset widget values to style defaults
  ;;
  (defun style_act (index / style-list)
    (setq style-idx (atoi index))
    (setq tstyle (nth style-idx slist))
    (setq style-idx (itoa style-idx))
    (set_tile "style" style-idx)
    (setq style-list (tblsearch "style" tstyle))
    (setq shght (cdr (assoc 40 style-list)))
    (if (/= shght 0)
      (progn
        (setq hght shght)
        (set_tile "hght" (ai_rtos hght))
      )
    )
    (setq wid (cdr (assoc 41 style-list)))
    (set_tile "wid" (ai_rtos wid))
    (setq obl (cdr (assoc 50 style-list)))
    (set_tile "obl" (ai_angtos obl))
    (setq bk-up (cdr (assoc 71 style-list)))
    (if (= (logand bk-up 2) 2)
      (set_tile "bkwd" (itoa (setq bkwd 1)))
      (set_tile "bkwd" (itoa (setq bkwd 0)))
    )
    (if (= (logand bk-up 4) 4)
      (set_tile "upsd" (itoa (setq upsd 1)))
      (set_tile "upsd" (itoa (setq upsd 0)))
    )
  )
  ;;
  ;; Justification action.  Set vertical and horizontal alignment variables,
  ;; grey out rotation and height if alignment = "aligned", grey out rotation
  ;; if alignment = "fit".
  ;;
  (defun jlist_act (index / templist)
    (setq just-idx (atoi index))
    (cond
      ((= just-idx 0) (setq va 0 ha 0))
      ((= just-idx 1) (setq va 0 ha 1))
      ((= just-idx 2) (setq va 0 ha 2))
      ((= just-idx 3) (setq va 0 ha 3))
      ((= just-idx 4) (setq va 0 ha 4))
      ((= just-idx 5) (setq va 0 ha 5))
      ((= just-idx 6) (setq va 3 ha 0))
      ((= just-idx 7) (setq va 3 ha 1))
      ((= just-idx 8) (setq va 3 ha 2))
      ((= just-idx 9) (setq va 2 ha 0))
      ((= just-idx 10) (setq va 2 ha 1))
      ((= just-idx 11) (setq va 2 ha 2))
      ((= just-idx 12) (setq va 1 ha 0))
      ((= just-idx 13) (setq va 1 ha 1))
      ((= just-idx 14) (setq va 1 ha 2))
    )
    (if (or (= ha 3) (= ha 5))  ; If Aligned or Fit text
      (mode_tile "rot" 1)
      (mode_tile "rot" 0)
    )
    (if (= ha 3)                ; If Aligned text
      (mode_tile "hght" 1)
      (mode_tile "hght" 0)
    )
    (if (= ha 5)                ; If Fit text
      (mode_tile "wid" 1)
      (mode_tile "wid" 0)
    )
    ;; Reset rotation and height if changing from aligned.
    (if (and (= ha-prev 3)  (/= ha 3))
      (progn
        (set_tile "rot"  (ai_angtos (setq rot 0.0)))
        (set_tile "hght" (ai_rtos (setq hght 1.0)))
      )
    )

    ;; Reset rotation and width if changing from fit.
    (if (and (= ha-prev 5) (/= ha 5))
      (progn
        (set_tile "rot" (ai_angtos (setq rot 0.0)))
        (set_tile "wid" (ai_rtos (setq wid 1.0)))
      )
    )

    (setq ha-prev ha)           ; update ha-prev for next time
    (setq just-idx (itoa just-idx))
  )
  ;;
  ;; Set intitial alignment setting based on vertical and horizontal alignment
  ;; bit codes.
  ;;
  (defun set_just_idx ()
    (cond
      ((= ha 0)             ; Horiz alignment = Left
        (cond
          ((= va 0) (setq just-idx "0"))
          ((= va 1) (setq just-idx "12"))
          ((= va 2) (setq just-idx "9"))
          ((= va 3) (setq just-idx "6"))
        )
      )
      ((= ha 1)             ; Horiz alignment = Center
        (cond
          ((= va 0) (setq just-idx "1"))
          ((= va 1) (setq just-idx "13"))
          ((= va 2) (setq just-idx "10"))
          ((= va 3) (setq just-idx "7"))
        )
      )
      ((= ha 2)             ; Horiz alignment = Right
        (cond
          ((= va 0) (setq just-idx "2"))
          ((= va 1) (setq just-idx "14"))
          ((= va 2) (setq just-idx "11"))
          ((= va 3) (setq just-idx "8"))
        )
      )
      ((= ha 3) (setq just-idx "3"))   ; Aligned
      ((= ha 4) (setq just-idx "4"))   ; Middle
      ((= ha 5) (setq just-idx "5"))   ; Fit
      (T (setq just-idx "0"))
    )
    just-idx
  )

  (defun modify_text ()
    ;;  insertion point
    (setq showpt (list x1 y1 z1))
    (setq bit-10 (trans showpt 1 ename))
    ;;  alignment point
    ;;  for 'Aligned' or 'Fit', alignment point must be different
    ;;  for all others, use insertion point
    ;;  (ACAD will recompute insertion point)
    (if (or (= ha 3) (= ha 5))
      (progn
        ;;  if no alignment point, fabricate one
        (if (not alipt)
          ;;  add text width to insertion point
          (setq alipt
            (list (+ (car showpt) (car (cadr (textbox elist))))
                  (cadr showpt)
                  (caddr showpt)
            )
          )
        )
        (setq bit-11 (trans alipt 1 ename))
      )
      (setq bit-11 bit-10)
    )
    (modify_properties)
    (tempmod tstyle 7 nil)
    (tempmod bit-10 10 nil)
    (tempmod bit-11 11 nil)
    (tempmod text 1 nil)
    (tempmod hght 40 nil)
    (tempmod wid 41 nil)
    (tempmod rot 50 nil)
    (tempmod obl 51 nil)
    (setq bk-up (+ (* bkwd 2) (* upsd 4)))
    (tempmod bk-up 71 nil)
    (tempmod ha 72 nil)
    ;; Attdefs use 74, text 73
    (if (= etype "ATTDEF")
      (progn
        (tempmod attag 2 nil)
        (tempmod atprompt 3 nil)
        (setq icvp (+ inv (* 2 con) (* 4 vfy) (* 8 pre)))
        (tempmod icvp 70 nil)
        (tempmod va 74 nil)
      )
      (tempmod va 73 nil)
    )
    (entmod elist)
  )

  (defun ddtext (/ 2ndpt slist i)
    (if (= etype "TEXT")
      (if (not (new_dialog "ddtext" dcl_id)) (exit))
      (if (not (new_dialog "ddattdef" dcl_id)) (exit))
    )
    (set_tile_props)
    (set_tile_handle)
    (set_tile_text)
    (set_tile_tag)
    (set_tile_prompt)
    (set_tile_hght)
    (set_tile_wid)
    (set_tile_rot)
    (set_tile_obl)
    (set_tile_bk-up)
    (set_tile_icvp)
    (set_tile_style)
    (set_tile_just)
    (setq pt1 (trans (cdr (assoc 10 elist)) ename 1))
    (if (not (assoc 11 elist))
      (progn (setq pt2 pt1)
        (setq elist (cons (cons '11 (cdr (assoc 10 elist))) elist ))
      )
;;(trans '(0.0 0.0 0.0) ename 1))
      (setq pt2 (trans (cdr (assoc 11 elist)) ename 1))
    )
    (if (or (and (= ha 0) (= va 0))
            (= ha 3)
            (= ha 5)
        )
      (setq showpt pt1)
      (setq showpt pt2)
    )
    (if (or (= ha 3) (= ha 5))
      (setq alipt pt2)
      (setq alipt nil)
    )

    (set_tile "x1_pt" (ai_rtos (setq x1 (car showpt))))
    (set_tile "y1_pt" (ai_rtos (setq y1 (cadr showpt))))
    (set_tile "z1_pt" (ai_rtos (setq z1 (caddr showpt))))

    ;; Define action for tiles
    (set_action_tiles)
    ;; Set focus initially to the text edit box.
    (if (not i) (progn (mode_tile "t_string" 2)(setq i 1)))
    (setq dialog-state (start_dialog))
    (if (= dialog-state 0)
      (reset)
    )
    (if (= dialog-state 1)
      (modify_text)
    )
    (if (= dialog-state 3)
      (progn
        (modify_text)
        (if (or (= ha 3) (= ha 5))
          (progn
            (setq showpt (getpoint (list x1 y1 z1) "\nFirst point: "))
            (if (not showpt)
              (setq showpt (list x1 y1 z1))
            )
            (setq 2ndpt (getpoint showpt "\nSecond point: "))
            (if 2ndpt
              (progn
                (setq alipt 2ndpt)
                (tempmod showpt 10 1)
                (tempmod alipt 11 1)
                (entmod elist)
              )
            )
            (setq elist (entget ename))
          )
          (progn
            (setq showpt (getpoint (list x1 y1 z1) "\nInsertion point: "))
            (if showpt
              (progn
                (if (and (= ha 0) (= va 0))
                  (tempmod showpt 10 1)
                  (tempmod showpt 11 1)
                )
                (entmod elist)
              )
              (setq showpt (list x1 y1 z1))
            )
          )
        )
        (ddtext)
      )
    )
  )

  ;;
  ;; Modify MTEXT
  ;;
  (defun modify_mtext ()
    (modify_properties)
    (setq pt1 (list x1 y1 z1))
    (tempmod pt1 10 0)
    (tempmod text 1 nil)
    (tempmod tstyle 7 nil)
    (tempmod just-idx 71 nil)
    (cond
      ((= dir-idx 0) (setq dir-idx 1))
      ((= dir-idx 1) (setq dir-idx 3))
      ((= dir-idx 2) (setq dir-idx 5))
      (T (setq dir-idx 1))
    )
    (tempmod dir-idx 72 nil)
    (tempmod hght 40 nil)
    (tempmod wid 41 nil)
    (tempmod rot 50 nil)
    (entmod elist)
  )

  ;; Set MText text style
  (defun MText_style (index / style-list)
    (setq style-idx (atoi index))
    (setq tstyle (nth style-idx slist))
    (setq style-idx (itoa style-idx))
    (set_tile "style" style-idx)
    (setq style-list (tblsearch "style" tstyle))
    (setq shght (cdr (assoc 40 style-list)))
    (if (/= shght 0)
      (progn
        (setq hght shght)
        (set_tile "hght" (ai_rtos hght))
      )
    )
  )

  ;; Run DDEDIT on given entity, working around possible re-entrancy
  ;; problems with MTEXTED
  (defun safe_ddedit (ename / orgMTextEd work)
    (setq orgMTextEd (getvar "MTEXTED"))
    (setq work orgMTextEd)
    (if (= ":" (substr work 1 1))
      (progn
        (while (and (< 0 (strlen work)) (/= "#" (substr work 1 1)))
          (setq work (substr work 2))
        )
        (if (= "#" (substr work 1 1))
          (setq work (substr work 2))
        )
        (setvar "MTEXTED" work)
      )
    )
    (verify_arxapp_loaded "acmted.arx") ;; make sure it's loaded...
    (command "_DDEDIT" ename "")
    (setvar "MTEXTED" orgMTextEd)
  )

  ;; Verify MText object width
  (defun ver_MtextWidth (value)
    (setq value (distof value))
    (set_tile "MTextWidth" (ai_rtos value))
    (set_tile "error" "")
    (if (< value 0.0)
      (set_tile "error" "Value must be zero or positive.")
      (setq wid value)
    )
  )

  (defun ddmtext ( / gc3)
    ;; Get dialog
    (if (not (new_dialog "ddmtext" dcl_id)) (exit))

    ;; Load list boxes
    (start_list "MTextJustify")
    (mapcar 'add_list '(
                        "Top Left"
                        "Top Center"
                        "Top Right"
                        "Middle Left"
                        "Middle Center"
                        "Middle Right"
                        "Bottom Left"
                        "Bottom Center"
                        "Bottom Right"
                       )
    )
    (end_list)

    (start_list "MTextDirection")
    (mapcar 'add_list '(
                        "Horizontal"
                        "Vertical"
                        "By Style"
                       )
    )
    (end_list)

    ;; Set initial tile values
    (set_tile_props)
    (set_tile_handle)
    (set_tile_pt1 0)
    (set_tile_style)
    (setq just-idx (cdr (assoc 71 elist)))
    (set_tile "MTextJustify" (itoa (1- just-idx)))
    (setq dir-idx (cdr (assoc 72 elist)))
    (cond
      ((= dir-idx 1) (setq dir-idx 0))
      ((= dir-idx 2) (setq dir-idx 0))
      ((= dir-idx 3) (setq dir-idx 1))
      ((= dir-idx 4) (setq dir-idx 1))
      ((= dir-idx 5) (setq dir-idx 2))
      (T (setq dir-idx 0))
    )
    (set_tile "MTextDirection" (itoa dir-idx))
    (set_tile "MTextWidth" (ai_rtos (setq wid (cdr (assoc 41 elist)))))
    (set_tile_hght)
    (set_tile_rot)

    ;; Set edit box, disable if too long
    (setq gc3 (cdr (assoc 3 elist))
          text (cdr (assoc 1 elist)))
    (if gc3
      (progn  ;; Text is over 250 chars, disable edit tile
        (set_tile "t_string" (strcat (substr gc3 1 25) " ..."))
        (mode_tile "t_string" 1)
      )
      (if (> (strlen text) 80)
        (progn  ;; Still too big
          (set_tile "t_string" (strcat (substr text 1 25) " ..."))
          (mode_tile "t_string" 1)
        )
        (set_tile "t_string" text)
      )
    )

    ;; Define action for tiles
    (set_action_tiles)
    (action_tile "style" "(MText_style $value)")
    (action_tile "MTextWidth" "(ver_MtextWidth $value)")
    (action_tile "MTextEdit" "(done_dialog 4)")
    (action_tile "MTextJustify" "(setq just-idx (1+ (atoi $value)))")
    (action_tile "MTextDirection" "(setq dir-idx (atoi $value))")

    ;; Set initial focus to text edit box.
    (mode_tile "t_string" 2)

    ;; Run the dialog
    (setq dialog-state (start_dialog))
    (cond
      ;; Cancelled - restore saved data
      ((= dialog-state 0) (reset))
      ;; OK - save new data
      ((= dialog-state 1) (modify_mtext))
      ;; Pick new insertion point
      ((= dialog-state 3)
        (modify_mtext)
        (setq pt1 (getpoint (list x1 y1 z1)  "\nNew Insertion Point: "))
        (ver_pt1 0)
        (ddmtext)
      )
      ;; Run full editor
      ((= dialog-state 4)
        (modify_mtext)
        (safe_ddedit ename)
        (setq elist (entget ename))
        (ddmtext)
      )
      (T nil)
    )
  )

  ;;
  ;; Modify VIEWPORT
  ;;

  (defun ddvport ()
    (if (not (new_dialog "ddvport" dcl_id)) (exit))
    (set_tile_props)
    (set_tile_handle)
    (setq vpt (cdr (assoc 10 elist)))
    (set_tile "xtext" (rtos (setq x1 (car vpt))))
    (set_tile "ytext" (rtos (setq y1 (cadr vpt))))
    (set_tile "ztext" (rtos (setq z1 (caddr vpt))))
    (setq wid (cdr (assoc 40 elist)))
    (set_tile "wid" (rtos wid))
    (setq hght (cdr (assoc 41 elist)))
    (set_tile "hght" (rtos hght))
    (setq vpid (cdr (assoc 69 elist)))
    (set_tile "vpid" (itoa vpid))
    (setq on-off (cdr (assoc 68 elist)))
    (cond
      ((= on-off 0) (set_tile "on-off" "OFF"))
      ((> on-off 0) (set_tile "on-off" "ON and Active"))
      (T (set_tile "on-off" "ON and Inactive"))
    )

    ;; Define action for tiles
    (set_action_tiles)

    (setq dialog-state (start_dialog))
    (if (= dialog-state 0)
      (reset)
    )
    (if (= dialog-state 1)
      (progn
        (if (= ecolor   0) (setq ecolor "BYBLOCK"))
        (if (= ecolor 256) (setq ecolor "BYLAYER"))
        (command "_.chprop" ename ""
                 "_la" elayer
                 "_c" ecolor ""
        )
      )
    )
  )
  ;;
  ;; Modify POLYLINE
  ;;
  (defun modify_polyline ()
    (modify_properties)
    (if (= ltgen "1")
        (if (/= (logand bit70 128) 128)
            (setq bit70 (+ bit70 128))
        )
    )
    (if (= ltgen "0")
        (if (= (logand bit70 128) 128)
            (setq bit70 (- bit70 128))
        )
    )
    (setq elist (subst (cons 70 bit70) (assoc 70 elist) elist))
    (entmod elist)
    ;; Added to take care of updating Vertex information for color
    ;; and linetype.
    (setq save-ename ename save-elist elist)
    (if (= (cdr (assoc 0 elist)) "LWPOLYLINE")
        (progn
            (emod ecolor 62)
            (emod eltype 6)
            (emod eltscale 48)
            (entmod elist)
        )
        (progn
            (setq ename (entnext save-ename))
            (setq elist (entget ename))

            (while (not (= (cdr (assoc 0 elist)) "SEQEND"))
                (emod ecolor 62)
                (emod eltype 6)
                (emod eltscale 48)
                (entmod elist)
                (setq ename (entnext ename))
                (setq elist (entget ename))
            )
        )
    )
    ;; Update the SEQEND
    (if (= (cdr (assoc 0 elist)) "SEQEND")
      (progn
        (emod ecolor 62)
        (emod eltype 6)
        (emod eltscale 48)
        (entmod elist)
      )
    )
    ;; Go back to header.
    (setq ename save-ename elist save-elist)

    (entupd ename)
  )

  ;; Increment vertex.  Set tile values to next vertex
  ;;
  (defun next_vertex ()
    (if (= (cdr (assoc 0 vlist)) "LWPOLYLINE")
        (progn
            ;; If the counter reaches the number of vertices,
            ;; roll it over to zero again.
            (if (= ctr (cdr (assoc 90 vlist)))
                (setq ctr 0)
            )
	        (set_tile "ctr" (itoa (setq ctr (+ 1 ctr))))
            (set_tile_vpt pointype)
        )
        (progn
            (setq vname (entnext vname))
            (setq vlist (entget vname))
            (if (= (cdr (assoc 0 vlist)) "VERTEX")
                (progn
                    (set_tile "ctr" (itoa (setq ctr (+ 1 ctr))))
                    (set_tile_vpt pointype)
                )
                (progn
                    (setq vname (entnext ename))
                    (setq vlist (entget vname))
                    (set_tile_vpt pointype)
                    (set_tile "ctr" (itoa (setq ctr 1)))
                )
            )
        )
    )
  )

  (defun ddpline (/ oldecho)
    (if (not (new_dialog "ddpline" dcl_id)) (exit))
    (set_tile_props)
    (set_tile_handle)
    (setq bit70 (cdr (assoc 70 elist)))
    (setq bit75 (cdr (assoc 75 elist)))
    (cond
      ((= (logand bit70 8) 8)   ; 3DPOLY
        (set_tile "ptype" (setq pltype "3D polyline"))
        (setq pointype 0)       ; WCS or ECS point values
        (mode_tile "fit" 1)
        (mode_tile "mesh" 1)
        (mode_tile "bezier" 1)
        (mode_tile "ltgen" 1)
        (set_tile "none" "1")
        (set_tile_closed)
        (set_tile_fitsmooth)
      )
      ((= (logand bit70 16) 16) ; 3DMESH
        (set_tile "ptype" (setq pltype "3D mesh"))
        (setq pointype 0)
        (mode_tile "pline" 1)
        (mode_tile "fit" 1)
        (mode_tile "ltgen" 1)
        (setq m (1- (cdr (assoc 71 elist))))
        (setq n (1-(cdr (assoc 72 elist))))
        (setq u (1- (cdr (assoc 73 elist))))
        (if (< u 0) (setq u 0))
        (setq v (1- (cdr (assoc 74 elist))))
        (if (< v 0) (setq v 0))
        (set_tile "m" (itoa m))
        (set_tile "n" (itoa n))
        (set_tile "u" (itoa u))
        (set_tile "v" (itoa v))
        (set_tile_closed)
        (set_tile_fitsmooth)
      )
      ((= (logand bit70 64) 64) ; POLYFACE MESH
        (set_tile "ptype" (setq pltype "Polyface mesh"))
        (setq pointype 0)
        (mode_tile "f-s" 1)
        (mode_tile "mesh" 1)
        (mode_tile "pline" 1)
      )
      (T                        ; 2D POLYLINE
        (set_tile "ptype" (setq pltype "2D polyline"))
        (setq pointype 1)
        (mode_tile "bezier" 1)
        (mode_tile "mesh" 1)
        (if (= (logand bit70 128) 128)
            (set_tile "ltgen" (setq ltgen "1"))
        )
        (set_tile_closed)
        (set_tile_fitsmooth)
      )
    )

    (if (= (cdr (assoc 0 elist)) "LWPOLYLINE")
        (progn
            (if (not next) (setq vname ename))
            (setq next T)
            (set_tile "ctr" (itoa (setq ctr 1)))
            (setq vlist (entget ename))
        )
        (progn
            (if (not next) (setq vname (entnext ename)))
            (setq next T)
            (set_tile "ctr" (itoa (setq ctr 1)))
            (setq vlist (entget vname))
        )
    )
    (set_tile_vpt pointype)
    ;; Define action for tiles
    (set_action_tiles)
    (setq dialog-state (start_dialog))

    (if (= dialog-state 0)
      (reset)
    )
    (if (= dialog-state 1)
      (progn
        (modify_polyline)
        (if (or (= pltype "2D polyline")
                (= pltype "3D polyline")
            )
          (progn
            (command "_.pedit" ename)
            (if (= spltype 0) (command "_d"))
            (if (= spltype 1) (command "_f"))
            (if (or (= spltype 5)
                    (= spltype 6)
                )
              (progn
                (setvar "splinetype" spltype)
                (command "_s")
              )
            )
            (if (= closed "0")
              (command "_o")
              (command "_c")
            )
            (command "")

            (if (= spltype 0)
                (progn
                    (setq oldecho (getvar "cmdecho"))
                    (command "_cmdecho" 0)
                    (command "_convertpoly" "_light" ename "")
                    (command "_cmdecho" oldecho)
                )
            )
          )
        )
        (if (= pltype "3D mesh")
          (progn
            (command "_.pedit" ename)
            (if (= spltype 0) (command "_d"))
            (if (or (= spltype 5)
                    (= spltype 6)
                    (= spltype 8)
                )
              (progn
                (setvar "surftype" spltype)
                (setvar "surfu" u)
                (setvar "surfv" v)
                (command "_s")
              )
            )
            (if (/= closedm old-closedm)
              (command "_m")
            )
            (if (/= closedn old-closedn)
              (command "_n")
            )
            (command "")
          )
        )
      )
    )
  )
  ;;
  ;; All the spline data is contained in a single elist. We must do some
  ;; tricky list processing to loop through the elist in order to display
  ;; all of the control points.
  ;;
  ;; The structure of the elist is different for rational and non-rational
  ;; splines. Therefore, we check the rational spline flag.
  ;;
  (defun next_cntl_pt ()
    (setq elem-no 0)               ;; elem-no = element counter
    (if (= first-10-time 1)        ;; If first time, find location of first
        (foreach list_item elist   ;; cntl point element in elist
           (progn
               (setq elem-no (+ 1 elem-no))
               (if (= (car list_item) 10)
                   (progn
                       (if (= first-10-time 1)
                           (progn
                              (setq first-10-rec (- elem-no 1))
                              (setq first-10-time 0)
                              (if (= rational_spl_flag 1) ;; if rational
                                  (setq cur-10-rec (+ elem-no 1))
                                  (setq cur-10-rec elem-no)
                              )
                           )
                       )
                   )
               )
           )
        )
    )
    ;; Now we know the location of the first "10" record; it's stored
    ;; in first-10-rec. The first 10 record was already displayed when
    ;; the dialog first came up so let's display the second one when
    ;; the user presses the "next" button (the first time through).
    ;;
    (setq temprec (nth cur-10-rec elist))
    ;;
    (if (= (car temprec) 10)   ;; if 10 record
      (progn
        (if (= rational_spl_flag 1)    ;; if rational spline
          (progn
            (setq tempweight (nth (+ cur-10-rec 1) elist))
            (setq cur-10-rec (+ 2 cur-10-rec))
          )
          (progn                       ;; else
            (setq cur-10-rec (+ 1 cur-10-rec))
          )
        )
      )                                ;; end if rational spline
      (progn                   ;; else reset counters
        (setq temprec (nth first-10-rec elist))  ;; get 1st 10 rec
        (setq tempweight (nth (+ first-10-rec 1) elist))
        (setq cntl-pt-indicator 0)
        (if (= rational_spl_flag 1)             ;; if rational
            (setq cur-10-rec (+ first-10-rec 2)) ;; point to 2nd 10 rec
            (setq cur-10-rec (+ first-10-rec 1)) ;; point to 2nd 10 rec
        )
      )
    )                          ;; end if 10 record

    ;; Display cntl point, weight and ctr. Increment ctr.
    (setq cntl-pt (cdr temprec))
    (set_tile "xtext" (rtos (setq x1 (car cntl-pt))))
    (set_tile "ytext" (rtos (setq y1 (cadr cntl-pt))))
    (set_tile "ztext" (rtos (setq z1 (caddr cntl-pt))))
    (setq cntl-pt-indicator (+ 1 cntl-pt-indicator))
    (set_tile "cntl_ctr" (itoa cntl-pt-indicator))
    (if (= rational_spl_flag 1)                     ;; if rational
        (set_tile "weight" (rtos (cdr tempweight))) ;; disp wght
        (mode_tile "weight_text" 1)                 ;; disable wght
    )
  )
  ;;
  ;; All the spline info is contained in a single elist. We must do some
  ;; tricky list processing to loop through the elist in order to display
  ;; all of the user data points.
  ;;
  (defun next_data_pt ()
    (setq elem-no 0)               ;; elem-no = element counter
    (if (= first-11-time 1)        ;; If first time, find location of first
        (foreach list_item elist   ;; data point element in elist
           (progn
               (setq elem-no (+ 1 elem-no))
               (if (= (car list_item) 11)
                   (progn
                       (if (= first-11-time 1)
                           (progn
                              (setq first-11-rec (- elem-no 1))
                              (setq cur-11-rec elem-no)
                              (setq first-11-time 0)
                           )
                       )
                   )
               )
           )
        )
    )
    (setq temprec (nth cur-11-rec elist))
    ;; If it's not a DXF "11" element then we've gone past the last
    ;; "11" element. Go back to first "11" element. Reset counters.
    (if (= (car temprec) 11)
        (setq data-pt (cdr temprec))
        (progn                           ;; else
           (setq data-pt (cdr (nth first-11-rec elist)))
           (setq cur-11-rec first-11-rec)
           (setq data-pt-indicator 0)
        )
    )
    ;; Display data point and ctr. Increment counters.
    (set_tile "dxtext" (rtos (setq x1 (car data-pt))))
    (set_tile "dytext" (rtos (setq y1 (cadr data-pt))))
    (set_tile "dztext" (rtos (setq z1 (caddr data-pt))))
    (setq data-pt-indicator (+ 1 data-pt-indicator))
    (set_tile "data_ctr" (itoa data-pt-indicator))
    (setq cur-11-rec (+ 1 cur-11-rec))
  )
  ;;
  ;; Modify SPLINE
  ;;
  (defun modify_spline ()
    (modify_prop_geom)
    (entmod elist)
  )

  (defun ddspline ()
    (if (not (new_dialog "ddspline" dcl_id)) (exit))
    (set_tile_props)
    (set_tile_handle)
    (set_tile_spline_props)
    ;; Display first control point
    (set_tile_cntl_pt)
    ;; Display first data point
    (set_tile_data_pt)

    ;; Initialize flags to indicate first time through the dialog.
    ;; For control points and user data points the "next" buttons
    ;; in the dialog call the appropriate functions.
    (setq first-10-time 1)
    (setq first-11-time 1)
    ;; initialize control point number counter
    (setq cntl-pt-indicator 1)
    (setq data-pt-indicator 1)
    (set_tile "cntl_ctr" (itoa cntl-pt-indicator))
    (set_tile "data_ctr" (itoa data-pt-indicator))
    (set_action_tiles)
    (setq dialog-state (start_dialog))

    (if (= dialog-state 0)
      (reset)
    )
    (if (= dialog-state 1)
      (modify_spline)
    )
  )

  ;;
  ;; Modify DIMENSION
  ;;
  (defun ddimen (/ dtypebit blkname bename sublist a stname n dimtbl
                   dimsty dimlist dimtext svtext dimovr sv_dvlist stlist
                   dimtype dimsvcurset)
   (setq dimtype "DDIMEN"
          dimsvcurset (ddimen_dimsty_restore)
    )
    (if (not (new_dialog "ddimen" dcl_id)) (exit))
    (set_tile_props)
    (set_dimen_props)
    (setq dimtext (cdr (assoc 1 elist)))
    (set_tile "t_string" (if (= "" dimtext) "<>" dimtext))
    (set_tile_handle)
    ;; Define action for tiles
    (set_action_tiles)

    ;; Set initial focus to text edit box.
    (mode_tile "t_string" 2)

    (setq dialog-state (start_dialog))
    (if (= dialog-state 0)
      (if (= ddimmt 1)
        (reset)
      )
    )
    (if (= dialog-state 1)
      (progn
        (if (/= dimsty stname)
          (progn
            ; Be sure to add (3 . dimstylename) element. R12 *UNNAMED case
            ; doesn't have it.
            (if (null (assoc 3 elist))
              (setq elist (append elist (list (cons 3 dimsty))))
              ; else just replace it.
              (setq elist (subst (cons 3 dimsty) (assoc 3 elist) elist))
            )
            ; refresh sv_dvlist with new dimstyle.
            (setq sv_dvlist (tblsearch "dimstyle" dimsty))
          )
        )
        (if (not (null dimlist))   ; attempted to change dimvars
          (ddimen_complist sv_dvlist dimlist dimtbl)
        )
        ; Modify dimension text
        (if (/= dimtext text)
          (progn
            (setq dimtext (if (= text "<>") "" text))
            (setq elist (subst (cons 1 dimtext) (assoc 1 elist) elist))
          )
        )
        (modify_properties)
        (entmod elist)
      )
    )
    (if (= dialog-state 4)
      (progn
        (setq ddimmt 1)
        (if (/= dimsty stname)
          (progn
            ; Be sure to add (3 . dimstylename) element. R12 *UNNAMED case
            ; doesn't have it.
            (if (null (assoc 3 elist))
              (setq elist (append elist (list (cons 3 dimsty))))
              ; else just replace it.
              (setq elist (subst (cons 3 dimsty) (assoc 3 elist) elist))
            )
            ; refresh sv_dvlist with new dimstyle.
            (setq sv_dvlist (tblsearch "dimstyle" dimsty))
          )
        )
        (if (not (null dimlist))   ; attempted to change dimvars
          (ddimen_complist sv_dvlist dimlist dimtbl)
        )
        ; Modify dimension text
        (if (/= dimtext text)
          (progn
            (setq dimtext (if (= text "<>") "" text))
            (setq elist (subst (cons 1 dimtext) (assoc 1 elist) elist))
          )
        )
        (modify_properties)
        (entmod elist)
        (safe_ddedit ename)
        (setq elist (entget ename))
        (ddimen)
      )
    )
    (ddimen_setvars dimsvcurset)      ; Prepare to exit
  )

  ;;
  ;; Modify TOLERANCE
  ;;
  (defun ddtolerance (/ a stname n dimtbl
                   dimsty dimlist dimovr sv_dvlist stlist dimtype
                   dimsvcurset)
    (setq dimtype "DDTOLERANCE"
          dimsvcurset (ddimen_dimsty_restore)
    )
    (if (not (new_dialog "ddtolerance" dcl_id)) (exit))
    (set_tile_props)
    (set_dimen_props)
    (set_tile_handle)
    (set_action_tiles)
    (setq dialog-state (start_dialog))
    (if (= dialog-state 0)
      (if (= ddimmt 1)
        (reset)
      )
    )
    (if (= dialog-state 1)
      (progn
        (if (/= dimsty stname)
          (progn
            ; Be sure to add (3 . dimstylename) element. R12 *UNNAMED case
            ; doesn't have it.
            (if (null (assoc 3 elist))
              (setq elist (append elist (list (cons 3 dimsty))))
              ; else just replace it.
              (setq elist (subst (cons 3 dimsty) (assoc 3 elist) elist))
            )
            ; refresh sv_dvlist with new dimstyle.
            (setq sv_dvlist (tblsearch "dimstyle" dimsty))
          )
        )
        (if (not (null dimlist))   ; attempted to change dimvars
          (ddimen_complist sv_dvlist dimlist dimtbl)
        )
        (modify_prop_geom)
        (entmod elist)
      )
    )
    (if (= dialog-state 4)
      (progn
        (setq ddimmt 1)
        (if (/= dimsty stname)
          (progn
            ; Be sure to add (3 . dimstylename) element. R12 *UNNAMED case
            ; doesn't have it.
            (if (null (assoc 3 elist))
              (setq elist (append elist (list (cons 3 dimsty))))
              ; else just replace it.
              (setq elist (subst (cons 3 dimsty) (assoc 3 elist) elist))
            )
            ; refresh sv_dvlist with new dimstyle.
            (setq sv_dvlist (tblsearch "dimstyle" dimsty))
          )
        )
        (if (not (null dimlist))   ; attempted to change dimvars
          (ddimen_complist sv_dvlist dimlist dimtbl)
        )
        (modify_properties)
        (entmod elist)
        (safe_ddedit ename)
        (setq elist (entget ename))
        (ddtolerance)
      )
    )
    (ddimen_setvars dimsvcurset)      ; Prepare to exit
)

  ;;
  ;; ddimen_dlg - jump to ADS ddim module
  ;;

  (defun ddimen_dlg (dimtype tile dimsty dvlist / dimlist)

    ; Jump to DDIM with overrides if any.
    (cond ((= tile 11) (setq dimlist (c:ddim "DDMODIFY" dimtype tile dimsty dvlist))) ; Geometry
          ((= tile 12) (setq dimlist (c:ddim "DDMODIFY" dimtype tile dimsty dvlist))) ; Format
          ((= tile 13) (setq dimlist (c:ddim "DDMODIFY" dimtype tile dimsty dvlist))) ; Annotation
    )
  )

  ;;
  ;; Dimension variables updated are stored as overrides.
  ;;

  (defun ddimen_complist (dimolist dimnlist dimtbl / i odvar ndvar
                          dv dime commandIssued)

    ; Here we skip the following first three items:
    ;   (0 . "DIMSTYLE") (2 . sylename) (70 . 0)
    ;
    ; The list comparison immediately begins with dimvar.
    ;
    ;  dimolist - original states of dimvars
    ;  dimnlist - new dimvar list which is of the resbuf's
    ;  dime - entity name for the dimension
    ;  odvar - value of dimvar for the dimolist
    ;  ndvar - value of dimvar for the dimnlist

    (setq i 3
          dime (cdr (assoc -1 elist))
    )
    (while (setq odvar (cdr (nth i dimolist)))
      (progn
        (setq ndvar (cdr (nth i dimnlist)))
        (setq dv (nth i dimtbl))
        (if (eq dv "dimtxsty")
            (setq odvar (cdr (assoc 2 (entget odvar)))
                  ndvar (cdr (assoc 2 (entget ndvar)))
            )
        )

                ;
                ; In order for DIMOVERRRIDE to operate correctly with string
                ; based dimvars, we must pass "." versus NULL strings.
                ; In this case ndvar is the new dimvar value the user wishes
                ; to update on selected dimensions.
                ;
            (if (/= ndvar odvar)
              (if (= 'STR (type ndvar))
                (if (= ndvar "")
                      (setq ndvar "."))))

        (if (/= ndvar odvar)
          (progn
            (setq dv (strcat "_" dv))
            ;
            ; Start the dimoverride command
            ;
            (if (not commandIssued)
               (progn
                 (command "_.dimoverride")
                 (setq commandIssued "T")
               )
            )
            ;
            ; Issue dim overrides
            ;
            (if (and (or (= dv "_dimclrd") (= dv "_dimclrt") (= dv "_dimclre"))
                     (or (= ndvar 0) (= ndvar 256))
                )
              (progn
                (if (= 0 ndvar) (command dv "BYBLOCK"))
                (if (= 256 ndvar) (command dv "BYLAYER"))
              )
              (command dv ndvar)
            )
          )
        )
        (setq i (1+ i))
      )
    )
    ;
    ; Select Entity and terminate command
    ;
    (if commandIssued
        (command "" dime "")
    )
  )

  ;;
  ;; Get style name currently selected style name.
  ;;

  (defun ddimen_style (/ dimsty)
    (setq dimsty (nth (atoi (get_tile "mod_style")) stlist)
          dimlist (ddimen_getdimvars dimsty)
    )
    dimsty
  )

  ;;
  ;; Set dimvars
  ;;
  (defun ddimen_setvars (dimsvcurset / dv i)
    (setq dv (cdr (nth 1 dimsvcurset)))
    (command "_.dimstyle" "" dv)
    (setq i 3)
    (while (setq dv (nth i dimsvcurset))
      (progn
        (setvar (car dv) (cdr dv))
        (setq i (1+ i))
      )
    )
  )
  ;;
  ;;  End-of-dimension
  ;; ==================

  ;;
  ;; Sub-dialogues for properties.  Common to all object dialogues
  ;;
  ;; This function pops a dialogue box consisting of a list box,image tile, and
  ;; edit box to allow the user to select or type a color number.  It returns
  ;; the color number selected.
  (defun getcolor (/ old-idx colorno cname lay_clr)
    (if (= (get_tile "error") "")
      (progn
        ;; Get the color associated with this object's layer, for use
        ;; in the color swatch if the user selects color BYLAYER.
        (setq lay_clr (cdr (assoc 62 (tblsearch "layer" elayer))))
        (if (numberp (setq temp_color (acad_colordlg ecolor T lay_clr)))
          (progn
            (setq ecolor temp_color)
            (setcolor)
          )
          (setq testcolor temp_color)
        )
      )
    )
    ecolor
  )
  ;;
  ;; Function to set the color tiles.
  (defun setcolor()
    (cond
      ((= 0 ecolor)
        (set_tile "t_color" "BYBLOCK")
        (col_tile "show_image" 7 nil)    ; show BYBLOCK as white
      )
      ((= 1 ecolor)
        (set_tile "t_color" "1 red")
        (col_tile "show_image" 1 nil)
      )
      ((= 2 ecolor)
        (set_tile "t_color" "2 yellow")
        (col_tile "show_image" 2 nil)
      )
      ((= 3 ecolor)
        (set_tile "t_color" "3 green")
        (col_tile "show_image" 3 nil)
      )
      ((= 4 ecolor)
        (set_tile "t_color" "4 cyan")
        (col_tile "show_image" 4 nil)
      )
      ((= 5 ecolor)
        (set_tile "t_color" "5 blue")
        (col_tile "show_image" 5 nil)
      )
      ((= 6 ecolor)
        (set_tile "t_color" "6 magenta")
        (col_tile "show_image" 6 nil)
      )
      ((= 7 ecolor)
        (set_tile "t_color" "7 white")
        (col_tile "show_image" 7 nil)
      )
      ((= 256 ecolor)
        (set_tile "t_color" "BYLAYER")
        (col_tile "show_image" (bylayer_col) nil)
      )
      (T
        (set_tile "t_color" (itoa ecolor))
        (col_tile "show_image" ecolor nil)
      )
    )
  )
  ;;
  ;; This function pops a dialogue box consisting of a list box, image tile, and
  ;; edit box to allow the user to select or  type a linetype.  It returns the
  ;; linetype selected.
  ;;
  (defun getltype (/ old-idx ltname)
    (if (not lt-idx)
      (progn
        (make_lt_lists)             ; linetype lists - ltnmlst, mdashlist
        (cond
          ((= eltype "BYLAYER")
             (setq lt-idx (getindex "BYLAYER" ltnmlst)))
          ((= eltype "BYBLOCK")
             (setq lt-idx (getindex "BYBLOCK" ltnmlst)))
          (T (setq lt-idx (getindex eltype ltnmlst)))
        )
      )
    )

    (if (= (get_tile "error") "")
     (progn
      (if (not (new_dialog "setltype" dcl_id)) (exit))
      (start_list "list_lt")
      (mapcar 'add_list ltnmlst)  ; initialize list box
      (end_list)
      (setq old-idx lt-idx)
      (ltlist_act (itoa lt-idx))

      (action_tile "list_lt" "(ltlist_act $value)")
      (action_tile "edit_lt" "(ltedit_act $value)")
      (action_tile "accept" "(test_ok)")
      (action_tile "cancel" "(reset_lt)")

      (if (= (start_dialog) 1) ; User pressed OK
        (cond
          ((= lt-idx 0)
            (set_tile "t_ltype" (bylayer_lt))
            "BYLAYER"
          )
          ((= lt-idx 1)
            (set_tile "t_ltype" "BYBLOCK")
            "BYBLOCK"
          )
          (T  (set_tile "t_ltype" ltname) ltname)
        )
        eltype
      )
     )
     eltype
    )
  )
  ;;
  ;; Edit box entries end up here
  (defun ltedit_act (ltvalue)
    (setq ltvalue (xstrcase ltvalue))
    (if (or (= ltvalue "BYLAYER")
            (= ltvalue "BY LAYER"))
      (setq ltvalue "BYLAYER")
    )
    (if (or (= ltvalue "BYBLOCK")
            (= ltvalue "BY BLOCK"))
      (setq ltvalue "BYBLOCK")
    )
    (if (setq lt-idx (getindex ltvalue ltnmlst))
      (progn
        (set_tile "error" "")
        (ltlist_act (itoa lt-idx))
      )
      (progn
        (set_tile "error" "Invalid linetype.")
        (setq lt-idx old-idx)
;;        (mode_tile "edit_lt" 2)
;;        (mode_tile "edit_lt" 3)
      )
    )
  )
  ;;
  ;; List selections end up here.  Update the list box, edit box, and color
  ;; tile.
  ;;
  (defun ltlist_act (index / dashdata)
    (set_tile "error" "")
    (setq lt-idx (atoi index))
    (setq ltname (nth lt-idx ltnmlst))
    (setq dashdata (nth lt-idx mdashlist))
    (col_tile "show_image" 0 dashdata)
    (set_tile "list_lt" (itoa lt-idx))
    (set_tile "edit_lt" ltname)
  )
  ;;
  ;; Reset to original linetype when cancel it selected
  ;;
  (defun reset_lt ()
    (setq lt-idx old-idx)
    (done_dialog 0)
  )
  ;;
  ;; This function pops a dialogue box consisting of a list box,image tile, and
  ;; edit box to allow the user to select or type a layer name.  It returns the
  ;; layer name selected.  It also has a button to find the status (On, Off,
  ;; Frozen, etc.) of any layer selected.
  ;;
  (defun getlayer (/ old-idx layname on off frozth linetype)
    ;; Create layer list the first time the layer
    ;; dialogue is called.
    (if (not lay-idx)
      (progn
        (make_lay_lists)
        (setq lay-idx (getindex elayer laynmlst))
      )
    )

    (if (= (get_tile "error") "")
     (progn
      (if (not (new_dialog "setlayer" dcl_id)) (exit))
      (set_tile "cur_layer" (getvar "clayer"))
      (start_list "list_lay")
      (mapcar 'add_list laynmlst)  ; initialize list box
      (end_list)
      (setq old-idx lay-idx)
      (laylist_act (itoa lay-idx))
      (action_tile "list_lay" "(laylist_act $value)")
      (action_tile "edit_lay" "(layedit_act $value)")
      (action_tile "accept" "(test_ok)")
      (action_tile "cancel" "(reset_lay)")
      (if (= (start_dialog) 1) ; User pressed OK
        (progn
          (set_tile "t_layer" layname)
          (setq elayer layname)
          ;; If layer equals bylayer reset color tile
          (if (= ecolor 256)
            (col_tile "show_image" (bylayer_col) nil)
          )
          layname
        )
        elayer
      )
     )
     elayer
    )
  )
  ;;
  ;; Edit box selections end up here.  Convert layer entry to upper case.  If
  ;; layer name is valid, clear error string, call (laylist_act) function.
  ;; Else print error message.
  ;;
  (defun layedit_act (layvalue)
    (setq layvalue (xstrcase layvalue))
    (if (setq lay-idx (getindex layvalue laynmlst))
      (progn
        (set_tile "error" "")
        (laylist_act (itoa lay-idx))
      )
      (progn
        (set_tile "error" "Invalid layer name.")
;;        (mode_tile "edit_lay" 2)
;;        (mode_tile "edit_lay" 3)
        (setq lay-idx old-idx)
      )
    )
  )
  ;;
  ;; List entry selections end up here.
  ;;
  (defun laylist_act (index / layinfo color dashdata)
    ;; Update the list box, edit box, and color tile.
    (set_tile "error" "")
    (setq lay-idx (atoi index))
    (setq layname (nth lay-idx laynmlst))
    (setq layinfo (tblsearch "layer" layname))
    (setq color (cdr (assoc 62 layinfo)))
    (setq color (abs color))
    (setq colname (colorname color))
    (set_tile "list_lay" (itoa lay-idx))
    (set_tile "edit_lay" layname)
  )
  ;;
  ;; Reset to original layer when cancel is selected.
  ;;
  (defun reset_lay ()
    (setq lay-idx old-idx)
    (done_dialog 0)
  )
  ;;
  ;; Checks validity of thickness from edit box.
  (defun getthickness (value)
    (setq ethickness (verify_d "eb_thickness" value ethickness))
  )
  ;;
  ;; Copy of (getthickness) for ltscale.  If more, make this function
  ;; generic.
  (defun getltscale (value)
    (setq eltscale (verify_d "eb_ltscale" value eltscale))
  )
  ;;
  ;; This function makes a list called laynmlst which consists of all the layer
  ;; names in the drawing.  It also creates a list called longlist which
  ;; consists of strings which contain the layer name, color, linetype, etc.
  ;; Longlist is later mapped into the layer listbox.  Both are ordered the
  ;; same.
  ;;
  (defun make_lay_lists (/ layname sortlist name templist layer_number)
    (setq sortlist nil)
    (setq templist (tblnext "LAYER" T))
    (setq layer_number 1)
    (while templist
      ;; No xref dependent layers, please.
          (if (/= (logand 16 (cdr (assoc 70 templist))) 16)
            (progn
              (setq name (cdr (assoc 2 templist)))
          (setq sortlist (cons name sortlist))
        )
          )
          ; Get the next layer.
      (setq templist (tblnext "LAYER"))
      ;; Not dead message...
      (if (= (/ layer_number 50.0) (fix (/ layer_number 50.0)))
        (set_tile "error" (strcat "Collecting..." (itoa layer_number)))
      )
      (setq layer_number (1+ layer_number))
    )
    (set_tile "error" "")
    (if (>= (getvar "maxsort") (length sortlist))
      (progn
        (if (> layer_number 50)
          (set_tile "error" "Sorting...")
        )
        (setq sortlist (acad_strlsort sortlist))
      )
      (setq sortlist (reverse sortlist))
    )
    (set_tile "error" "")
    (setq laynmlst sortlist)
  )
  ;;
  ;; This function makes 2 list - ltnmlst & mdashlist.
  ;; Ltnmlst is a list of linetype names read from the symbol table.  Mdashlist
  ;; is list consisting of lists which define the linetype pattern - numbers
  ;; that indicate dots, dashes, and spaces taken from group code 49.  The list
  ;; corresponds to the order of names in ltnmlst.
  ;;
  (defun make_lt_lists (/ ltlist ltname)
    (setq mdashlist nil)
        (setq sortlist nil)
        (setq ltype_number 1)
    (setq ltlist (tblnext "LTYPE" T))
    ;;(setq ltname (cdr (assoc 2 ltlist)))
    ;;(setq ltnmlst (list ltname))
    (while ltlist
          ;; No xref dependent linetypes, please.
          (if (/= (logand 16 (cdr (assoc 70 ltlist))) 16)
            (progn
          (setq ltname (cdr (assoc 2 ltlist)))
          (setq sortlist (cons ltname sortlist))
            )
          )
          ;; Get the next linetype.
          (setq ltlist (tblnext "LTYPE"))

          ;; Not dead message...
      (if (= (/ ltype_number 50.0) (fix (/ ltype_number 50.0)))
        (set_tile "error" (strcat "Collecting..." (itoa ltype_number)))
      )
      (setq ltype_number (1+ ltype_number))

    )

        ;; Remove Collecting message.
        (set_tile "error" "")

    ;; Sort based on maxsort.
        (if (>= (getvar "maxsort") (length sortlist))
      (progn
        (if (> ltype_number 50)
          (set_tile "error" "Sorting...")
        )
        (setq sortlist (acad_strlsort sortlist))
      )
      (setq sortlist (reverse sortlist))
    )
    (set_tile "error" "")
    (setq ltnmlst sortlist)

    (foreach ltname ltnmlst
      (setq ltlist (tblsearch "LTYPE" ltname))
      (if (= ltname "CONTINUOUS")
        (setq mdashlist (append mdashlist (list "CONT")))
        (setq mdashlist
            (append mdashlist (list (add_mdash ltlist)))
        )
      )
    )
    (setq ltnmlst (cons "BYBLOCK" ltnmlst))
    (setq mdashlist  (cons nil mdashlist))
    (setq ltnmlst (cons "BYLAYER" ltnmlst))
    (setq mdashlist  (cons nil mdashlist))
  )
  ;;
  ;; Get all the group code 49 values for a linetype and put them in a list
  ;; (pen-up, pen-down info).
  ;;
  (defun add_mdash (ltlist1 / dashlist assoclist dashsize)
    (setq dashlist nil)
    (while (setq assoclist (car ltlist1))
      (if (= (car assoclist) 49)
        (progn
          (setq dashsize (cdr assoclist))
          (setq dashlist (cons dashsize dashlist))
        )
      )
      (setq ltlist1 (cdr ltlist1))
    )
    (setq dashlist (reverse dashlist))
  )
  ;;
  ;; Color a tile, draw linetype, and draw a border around it
  ;;
  (defun col_tile (tile color patlist / x y)
    (setq x (dimx_tile tile))
    (setq y (dimy_tile tile))
    (start_image tile)
    (fill_image 0 0 x y color)
    (if (= color 7)
      (progn
        (if patlist (drawpattern x (/ y 2) patlist 0))
        (tile_rect 0 0 x y 0)
      )
      (progn
        (if patlist (drawpattern x (/ y 2) patlist 7))
        (tile_rect 0 0 x y 7)
      )
    )
    (end_image)
  )
  ;;
  ;; Draw a border around a tile
  ;;
  (defun tile_rect (x1 y1 x2 y2 color)
    (setq x2 (- x2 1))
    (setq y2 (- y2 1))
    (vector_image x1 y1 x2 y1 color)
    (vector_image x2 y1 x2 y2 color)
    (vector_image x2 y2 x1 y2 color)
    (vector_image x1 y2 x1 y1 color)
  )
  ;;
  ;; Draw the linetype pattern in a tile.  Boxlength is the length of the image
  ;; tile, y2 is the midpoint of the height of the image tile, pattern is a
  ;; list of numbers that define the linetype, and color is the color of the
  ;; tile.
  ;;
  (defun drawpattern (boxlength y2 pattern color / x1 x2
                      patlist dash)
    (setq x1 0 x2 0)
    (setq patlist pattern)
    (setq fx 30)
    (if (= patlist "CONT")
      (progn (setq dash boxlength)
        (vi)
        (setq x1 boxlength)
      )
      (foreach dash patlist
        (if (> (abs dash) 2.5)
          (setq fx 2)
        )
      )
    )
    (while (< x1 boxlength)
      (if (setq dash (car patlist))
        (progn
          (setq dash (fix (* fx dash)))
          (cond
            ((= dash 0) (setq dash 1) (vi))
            ((> dash 0) (vi))
            (T
              (if (< (abs dash) 2)
               (setq dash 2)
              )
              (setq x2 (+ x2 (abs dash)))
            )
          )
          (setq patlist (cdr patlist))
          (setq x1 x2)
        )
        (setq patlist pattern)
      )
    )
  )
  ;;
  ;; Determain state of xclip
  ;; Returns the group 71 value of the spacial filter dictionary.
  ;; If the entity doesn't have a spacial filter dictionary, this
  ;; returns 0. If it does it will return 0 or 1 depending on the
  ;; current setting of the state of the clipping visibility.
  ;;
  (defun xclipon(elist)
    (setq hasclip T)
    (if (/= (assoc 360 elist) nil)
      (progn
        (setq tmp (entget(cdr(assoc 360 elist))))
        (if (/= nil (assoc 360 tmp))
          (progn
            (setq tmp (entget(cdr(assoc 360 tmp))))
            (if (/= nil (assoc 360 tmp))
              (progn
                (setq tmp (entget(cdr(assoc 360 tmp))))
                (if (/= nil (assoc 71 tmp))
                  (cdr(assoc 71 tmp))
				  (progn 
					(setq hasclip nil)
					(eval 0)
				  )
                )
              )
			  (progn 
				(setq hasclip nil)
				(eval 0)
			  )
            )
          )
		  (progn 
			(setq hasclip nil)
			(eval 0)
		  )
        )
      )
	  (progn 
		(setq hasclip nil)
		(eval 0)
	  )
    )
  )
  ;;
  ;; Draw a dash or dot in image tile
  ;;
  (defun vi ()
    (setq x2 (+ x2 dash))
    (vector_image x1 y2 x2 y2 color)
  )
  ;;
  ;; If an item is a member of the list, then return its index number, else
  ;; return nil.
  ;;
  (defun getindex (item itemlist / m n)
    (setq n (length itemlist))
    (if (> (setq m (length (member item itemlist))) 0)
      (- n m)
      nil
    )
  )
  ;;
  ;; This function is called if the linetype is set "BYLAYER". It finds the
  ;; ltype of the layer so it can be displayed  beside the linetype button.
  ;;
  (defun bylayer_lt (/ layname layinfo ltype)
    (if lay-idx
      (progn
        (setq layname (nth lay-idx laynmlst))
        (setq layinfo (tblsearch "layer" layname))
        (setq ltype (cdr (assoc 6 layinfo)))
        "BYLAYER"
      )
      "BYLAYER"
    )
  )
  ;;
  ;; This function is called if the color is set "BYLAYER".  It finds the color
  ;; of the layer so it can be displayed beside the color button.
  ;;
  (defun bylayer_col (/ layname layinfo color)
    (setq layinfo (tblsearch "layer" elayer))
    (setq color (abs (cdr (assoc 62 layinfo))))
  )
  ;;
  ;; Used to set the color name in layer subdialogue.
  ;;
  (defun colorname (colnum / cn)
    (setq cn (abs colnum))
    (cond ((= cn 1) "red")
          ((= cn 2) "yellow")
          ((= cn 3) "green")
          ((= cn 4) "cyan")
          ((= cn 5) "blue")
          ((= cn 6) "magenta")
          ((= cn 7) "white")
          (T (itoa cn))
    )
  )
  ;;
  ;; If their is no error message, then close the dialogue.
  ;;
  (defun dismiss_dialog (action)
    (if (= action 0)
      (done_dialog 0)
      (if (= (get_tile "error") "")
        (done_dialog action)
      )
    )
  )

  (defun test_ok ()
    (if (= (get_tile "error") "")
      (done_dialog 1)
    )
  )

  (defun cancel ()
    (done_dialog 0)
  )

;;; =======================================================================
;;; SETUP layer and linetype lists for application, and initialize all
;;; program variables.

  (setq elist       (entget ename)
        old-elist   elist
        modlist     elist
        etype       (strcase (cdr (assoc 0 elist)))
        ecolor      (cdr (assoc 62 elist))
        elayer      (cdr (assoc 8 elist))
        eltscale    (cdr (assoc 48 elist))
        ethickness  (cdr (assoc 39 elist))
        eltype      (cdr (assoc 6 elist))
  )
  (if (= (assoc 210 elist) nil)
    (setq extru (list 0.0 0.0 1.0))
    (setq extru (cdr (assoc 210 elist)))
  )

  (if (not ecolor) (setq ecolor 256))
  (if (not eltype) (setq eltype "BYLAYER"))
  (if (not ethickness) (setq ethickness 0))
  (if (not eltscale) (setq eltscale 1))
) ; end ddmodify_init

;;; --------------------------------------------------------------------------
;;; Function: DDMODIFY_SELECT
;;;
;;; Object aquisition function.
;;;
;;; (ddmodify_select)
;;;
;;; Obtains object to be modified, in one of three ways:
;;;
;;;   1 - Autoselected.
;;;   2 - Prompted for.
;;;   3 - Passed as an argument in a call to (ddmodify <ename> )
;;;
;;; The (ddmodify_select) function also sets the value of the
;;; global symbol AI_SELTYPE to one of the above three values to
;;; indicate the method thru which the object was aquired.
;;;
;;; This value can be useful to applications that want to RESTORE
;;; an object that was autoselected to its previous selected state
;;; when they terminate, although there doesn't appear to be any
;;; way to do this right now.

(defun ddmodify_select ()
   (cond
      (  ename                             ; (ddmodify) was called
         (cond                             ; with an <ename> argument
            (  (entget ename)              ;   If object is non-deleted
               (setq ai_seltype 3)         ;   then return its ename.
               (ai_return ename))))

      ;; return auto-selected , see ai_utils.lsp
      (  (ai_autossget1 "\nSelect one object to modify: "))

      (t (princ "\nNothing selected.")
         (ai_return nil))
   )
)

;;; ============= Command line interface function =======================

(defun C:DDMODIFY ()
   (ddmodify nil)
   (princ)
)

;;; ================== (ddmodify) - Main program ========================
;;;
;;; (ddmodify <ename> )
;;;
;;; Main program function, callable as a subroutine.
;;;
;;; <ename> = object name of the object to modify.
;;;
;;; If <ename> is nil, then user is prompted to select
;;; the object interactively.
;;;
;;; Before (ddmodify) can be called as a subroutine, it must
;;; be loaded first.  It is up to the calling application to
;;; first determine this, and load it if necessary.


(defun ddmodify (ename /
2ndpt              ell_calc_area            move_pt1               templist
add_mdash          ell_tile                 n                      tempmod
alipt              eltscale                 name                   tempst_ang
ang                eltype                   newpoint               test_ok
arc_calc           emod                     next                   text
arclen             end_ang                  next_vertex            th-value
assoclist          endpt                    obl                    tile
atprompt           errchk                   off                    tile_rect
attag              ethickness               old_majrad             tilemode
attprompt          etype                    old-closed             totang
bit                extru                    old-closedm            tstyle
bit1               fchk                     old-closedn            u
bit-10             first-10-rec             old-elist              undo_init
bit-11             first-10-time            olderr                 upsd
bit2               first-11-rec             old-fit                v
bit3               first-11-time            old-idx                va
bit4               fit                      oldlist                value
bit70              frozth                   old-spltype            ver_4
bit-70             f-vis                    old-u                  ver_ang1
bit75              fx                       old-v                  ver_ang2
bk-up              get_color                on                     ver_col
bkwd               getcolor                 onoff                  ver_colsp
boxlength          getindex                 on-off                 ver_eangle
bylayer_col        getlayer                 patlist                ver_hght
bylayer_lt         getltype                 pattern                ver_majrad
calc               getthickness             pltype                 ver_obl
cancel             globals                  polytype               ver_pt1
cir_calc           ha                       pre                    ver_pt2
closed             ha-prev                  proplist               ver_pt3
closedm            help_entry               pt                     ver_pt4
closedn            hght                     pt1                    ver_rad
cmd                icvp                     pt1_eq_pt2             ver_rot
cn                 image_add_vector         pt2                    ver_row
cname              image_clean_variables    pt3                    ver_rowsp
cntl-pt-indicator  image_cross_product      pt4                    ver_tag
code_71            image_disp_opt           ptype                  ver_u
col_tile           image_dot_product        radius                 ver_v
col-idx            image_normalize_vector   rational_spl_flag      ver_wid
colname            image_rotate_vector      reset                  ver_x1
colnmlst           image_scale              reset_flag             ver_x2
colnolst           image_scale_vector       reset_lay              ver_x3
colnum             image_update             reset_lt               ver_x4
color              index                    reset_uv               ver_xline_pt1
colorname          inv                      rot                    ver_xline_pt2
colorno            item                     rows                   ver_xline_x1
col-sp             item1                    row-sp                 ver_xline_x2
columns            item2                    rrat                   ver_xline_y1
con                itemlist                 s                      ver_xline_y2
coord              jlist                    set_action_tiles       ver_xline_z1
ctr                jlist_act                set_just_idx           ver_xline_z2
cur-10-rec         just-idx                 set_tile_bk-up         ver_xscl
cur-11-rec         layedit_act              set_tile_cntl_pt       ver_y1
cvpname            lay-idx                  set_tile_data_pt       ver_y2
dash               layinfo                  set_tile_dirv          ver_y3
dashdata           laylist                  set_tile_edges         ver_y4
dashlist           laylist_act              set_tile_endang        ver_yscl
dashsize           layname                  set_tile_hght          ver_z1
data-pt-indicator  laynmlst                 set_tile_icvp          ver_z2
dcl_id             layvalue                 set_tile_just          ver_z3
dd3dface           line_calc                set_tile_obl           ver_zscl
dd3dsolid          linetype                 set_tile_prompt        verify_a
ddarc              list1                    set_tile_props         verify_d
ddblock            longlist                 set_tile_pt1           verify_i
ddbody             ltabstr                  set_tile_pt2           verify_xline
ddcircle           ltedit_act               set_tile_pt3           vfy
ddellipse          ltidx                    set_tile_pt4           vi
ddgetprompt        lt-idx                   set_tile_rad           vlist
ddgettext          ltlist                   set_tile_rc            vname
ddimage            ltlist_act               set_tile_rot           vpf
ddimen             ltlist1                  set_tile_scale         vpid
ddleader           ltname                   set_tile_spline_props  vpldata
ddline             ltnmlst                  set_tile_stang         vpn
ddlist             ltvalue                  set_tile_style         vpt
ddmline            ltype                    set_tile_tag           which_tiles
ddmodify_err       m                        set_tile_text          wid
ddmtext            majrad                   set_tile_vpt           x
ddpline            make_lay_lists           set_tile_wid           x1
ddpoint            make_lt_lists            set_tile_xline_pt1     x2
ddray              mdashlist                set_tile_xline_pt2     x3
ddregion           minrad                   setcolor               x4
ddshape            modify_3dface            shght                  xdlist
ddsolid            modify_3dsolid           showpt                 xline_pt1
ddspline           modify_arc               size                   xline_pt2
ddtext             modify_block             slist                  xline_x1
ddvport            modify_body              sname                  xline_x2
ddxline            modify_circle            sortlist               xline_y1
denom              modify_ellipse           spltype                xline_y2
dialog-state       modify_image             ss                     xline_z1
dir_pt             modify_line              st_ang                 xline_z2
dir_ptx            modify_mline             stpt                   xscale
dir_pty            modify_mtext             style_act              xx
dir_ptz            modify_point             style-idx              y
dismiss_dialog     modify_polyline          style-list             y1
drawpattern        modify_prop_geom         tagval                 y2
echo               modify_properties        temp                   y3
ecolor             modify_ray               temp_color             y4
edge1              modify_region            temp_dir_x             yscale
edge2              modify_shape             temp_dir_y             yy
edge3              modify_solid             temp_dir_z             z1
edge4              modify_spline            temp_xline_pt1         z2
edgetest           modify_text              temp_xline_x1          z3
elayer             modify_vport             temp_xline_y1          z4
elist              modify_xline             temp_xline_z1          zscale
ell_calc           modlist                  tempend_eang           zz
dir-idx            safe_ddedit              ver_MtextWidth         xcliponoff
MText_style
  )

  (setq old_cmd (getvar "cmdecho")    ; save current setting of cmdecho
        old_error  *error*            ; save current error function
        *error* ai_error              ; new error function
  )

  (setq old_pickstyle (getvar "PICKSTYLE"))          ; save old pickstyle
  (setq new_pickstyle (logand old_pickstyle (~ 1)))  ; turn off group selection
  (setvar "pickstyle" new_pickstyle)                 ; bit and set to new value

  (setvar "cmdecho" (cond (  (or (not *debug*) (zerop *debug*)) 0)
                          (t 1)))
  (cond
     (  (not (ai_notrans)))                      ; Not transparent?
     (  (not (ai_acadapp)))                      ; ACADAPP.EXP xloaded?
     (  (not (setq dcl_id (ai_dcl "ddmodify")))) ; is .DLG file loaded?
     (  (not (setq ename (ddmodify_select))))    ; object to modify?

     (t (ai_undo_push)
        (ddmodify_init)                          ; everything okay, proceed.
        (cond
           ((= etype "LEADER")
             (setq help_entry  "modify_Leader_dialog")
             (ddleader)
           )
           ((= etype "ARC")
             (setq help_entry  "modify_Arc_dialog")
             (ddarc)
           )
           ((= etype "ATTDEF")
             (setq help_entry  "modify_Attribute_Definition_dialog")
             (ddtext)
           )
           ((= etype "CIRCLE")
             (setq help_entry  "modify_Circle_dialog")
             (ddcircle)
           )
           ((= etype "ELLIPSE")
             (setq help_entry  "modify_Ellipse_dialog")
             (ddellipse)
           )
           ((= etype "3DSOLID")
             (setq help_entry  "modify_3d_Solid_dialog")
             (dd3dsolid)
           )
           ((= etype "BODY")
             (setq help_entry  "modify_Body_dialog")
             (ddbody)
           )
           ((= etype "REGION")
             (setq help_entry  "modify_Region_dialog")
             (ddregion)
           )
           ((= etype "HATCH")
             (setq help_entry  "modify_Hatch_dialog")
             (ddnewhatch)
           )
           ((= etype "SPLINE")
             (setq help_entry  "modify_Spline_dialog")
             (ddspline)
           )
           ((= etype "INSERT")    ; see ddblock for help_entry
             (ddblock)
           )
           ((= etype "LINE")
             (setq help_entry  "modify_Line_dialog")
             (ddline)
           )
           ((= etype "MLINE")
             (setq help_entry  "modify_multiLine_dialog")
             (ddmline)
           )
           ((= etype "RAY")
             (setq help_entry  "modify_Ray_dialog")
             (ddxline)
           )
           ((= etype "XLINE")
             (setq help_entry  "modify_Xline_dialog")
             (ddxline)
           )
           ((= etype "POINT")
             (setq help_entry  "modify_Point_dialog")
             (ddpoint)
           )
           ((or (= etype "POLYLINE") (= etype "LWPOLYLINE"))
             (setq help_entry  "modify_Polyline_dialog")
             ;; If a 2D pline, check to see if it is planar to the current
             ;; UCS, reject if not.   To see if the pline is parallel,
             ;; the 210 group (WCS) is added to the current UCS origin (WCS)
             ;; and then converted to the current UCS and checked to see if
             ;; it is equal to (0,0,1).

             ;; Incase the 210 is default and not in the dxf list.
             (if (= (assoc 210 (entget ename)) nil)
                (ddpline)
                (progn
                    (if (and (zerop (logand 120 (cdr (assoc 70 (entget ename)))))
                            (not (equal '(0.0 0.0 1.0)
                                   (trans (mapcar '+
                                             (cdr (assoc 210 (entget ename)))
                                             (trans '(0.0 0.0 0.0) 1 0)
                                          )
                                     0 1
                                   )
                                   0.0000000001            ; fuzz
                                )
                            )
                        )
                    (princ "\nThe 2D Polyline is not parallel to the current UCS.")
                    (ddpline)
                    )
                )
             )
           )

           ((= etype "SHAPE")
             (setq help_entry  "modify_Shape_dialog")
             (ddshape)
           )
           ((= etype "SOLID")
             (setq help_entry  "modify_Solid_dialog")
             (ddsolid)
           )
           ((= etype "TEXT")
             (setq help_entry  "modify_Text_dialog")
             (ddtext)
           )
           ((= etype "MTEXT")
             (setq help_entry  "modify_MText_dialog")
             (ddmtext)
           )
           ((= etype "TRACE")
             (setq help_entry  "modify_Trace_dialog")
             (ddsolid)
           )
           ((= etype "VIEWPORT")
             (setq help_entry  "modify_Viewport_dialog")
             (ddvport)
           )
           ((= etype "IMAGE")
             (setq help_entry  "modify_Image_dialog")
             (ddimage)
           )
           ((= etype "3DFACE")
             (setq help_entry  "modify_3D_Face_dialog")
             (dd3dface)
           )
           ((= etype "DIMENSION")
             (setq help_entry  "modify_Dimension_dialog")
             (ddimen)
           )
           ((= etype "TOLERANCE")
             (setq help_entry  "modify_Tolerance_dialog")
             (ddtolerance)
           )
;; Fall-through condition changed by MCAD for MDT 1.1 release.
;; This allows DDMODIFY to work on any custom object or any new object type
;; that isn't specifically handled above by calling the more generic DDCHPROP.
           (t
             (if (and (not ddchprop) (not (load "ddchprop" nil)))
                (princ (strcat "No dialog support for object type: " etype ".")))
             (progn
                (setq tempss (ssadd ename))
                (ddchprop tempss)
                (setq tempss nil)
             )
           )
        )
;; Previous fall-through condition.
;;           (t (princ (strcat "No dialog support for object type: "
;;                             etype "."
;;                     )
;;              )
;;           )
;;        )
        (ai_undo_pop)
     )
  )

  (if (eq new_pickstyle (getvar "PICKSTYLE")) ; if user didn't change pickstyle
      (setvar "PICKSTYLE" old_pickstyle)      ; transparently then set it back
  )                                           ; to what we started with

  (setq *error* old_error)
  (setvar "cmdecho" old_cmd)
  (if (not reset_flag)            ; if object was modified, then
      (ai_return ename)           ; return it's ename to caller
  )
)

(defun checkForLockedLayer (ename)
    (setq layername (cdr (assoc 8 (cdr (entget ename)))))
    (setq layerflag (cdr (assoc 70 (tblsearch "LAYER" layername))))
    (if (= layerflag 4)
        T
        nil
    )
)

(princ "  DDMODIFY loaded.  ")
(princ)
