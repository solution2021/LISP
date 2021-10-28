;;;=======================[ FlexDuct.lsp ]==============================
;;; Author: Copyright© 2007 Charles Alan Butler 
;;; Contact or Updates  @  www.TheSwamp.org
;;; Version:  1.7   Feb. 21,2008
;;; Purpose: Create Flex Duct from a centerline that the user picks
;;;    Centerline may be anything vla-curve will handle
;;; Sub_Routines:      
;;;    makePline which creates a LW Polyline
;;; Restrictions: UCS is supported
;;;    Duct Layer is hard coded, see var Flexlayer
;;;    No error handler at this time
;;; Known Issues:
;;;    Tight curves cause pline jacket distortion
;;;    Added warning when this is about to occur
;;; Returns:  none
;;;=====================================================================
;;;   THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED     ;
;;;   WARRANTY.  ALL IMPLIED WARRANTIES OF FITNESS FOR ANY PARTICULAR  ;
;;;   PURPOSE AND OF MERCHANTABILITY ARE HEREBY DISCLAIMED.            ;
;;;                                                                    ;
;;;  You are hereby granted permission to use, copy and modify this    ;
;;;  software without charge, provided you do so exclusively for       ;
;;;  your own use or for use by others in your organization in the     ;
;;;  performance of their normal duties, and provided further that     ;
;;;  the above copyright notice appears in all copies and both that    ;
;;;  copyright notice and the limited warranty and restricted rights   ;
;;;  notice below appear in all supporting documentation.              ;
;;;=====================================================================

(defun c:Flex(/         cl-ent    ribWidth  RibShort  RibLong   collar
               dist      steps     ribFlag   pt        curAng    curDer
               RibPtLst1 RibPtLst2 p1        p2        doc       space
               cflag     cl-len    ribRadius tmp       NewPline  NewPline2 
               pl1       pl2       cnt       errflag   InsulThick   FlexColor
               FlexLayer ss
              )
  (vl-load-com)
  (setq Doc (vla-get-activedocument (vlax-get-acad-object)))
  (vla-endundomark doc)
  (vla-startundomark doc)

  ;; \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
  
  ;;  Change these if you want
  
  (setq FlexLayer "0")   ; put your Duct layer here
  (setq FlexColor acred) ; put your color over ride here or Bylayer
  (setq InsulThick 0)    ; to be added to duct diameter, use 2 for 1" insulation
  (setq collar 6.0)      ; collar length at each end
  (setq DelCL nil)       ; delete the centerline t=Yes nil=No
  (setq GroupFlex t)     ; make flex duct a Group t=Yes nil=No
  
  ;; \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/


  ;;   --------   Local Functions   ---------

  ;;  Expects pts to be a list of 2D or 3D points
  (defun makePline (spc pts)
    (if (= (length (car pts)) 2) ; 2d point list
      (setq pts (apply 'append pts))
      (setq
        pts (apply 'append (mapcar '(lambda (x) (list (car x) (cadr x))) pts))
      )
    )
    (setq
      pts (vlax-make-variant
            (vlax-safearray-fill
              (vlax-make-safearray vlax-vbdouble (cons 0 (1- (length pts))))
              pts
            )
          )
    )
    (vla-addlightweightpolyline spc pts)
  )
  ;;   -------------------------------------


  ;;  Get the Duct Diameter, global variable
  (or duct:dia (setq duct:dia 16.0)) ; default value

  (while            ; Main Loop
    (progn
      (prompt (strcat "\nDuct diameter is set to "
                      (vl-princ-to-string duct:dia)
              )
      )
      (setvar "errno" 0) ; must pre set the errno to 0 
      (initget "Diameter")
      (setq cl-ent
             (entsel (strcat "\nSelect center line of flex duct.[Diameter]<"
                             (vl-princ-to-string duct:dia)
                             "> Enter to quit."))
      )

      (cond
        ((= (getvar "errno") 52) ; exit if user pressed ENTER
         nil        ; exit loop
        )
        ((= cl-ent "Diameter")
         (initget (+ 2 4))
         (setq
           tmp (getdist
                 (strcat "\nSpecify duct diameter <" (rtos duct:dia) ">: ")
               )
         )
         (and tmp (setq duct:dia tmp))
         t          ; stay in loop
        )

        ((not (null cl-ent))
         ;;  check entity before making the duct
         (if (not (vl-catch-all-error-p
                    (setq tmp (vl-catch-all-apply
                                'vlax-curve-getpointatparam
                                (list (car cl-ent) 0.0)
                              )
                    )
                  )
             )
           (progn   ; OK to make duct
             (setq cl-ent   (car cl-ent) ; Center Line
                   ribWidth (* duct:dia 0.167)
                   RibShort (+ duct:dia InsulThick) ; add insulation
                   RibLong  (+ RibShort (* ribWidth 2))
             )

             ;;  centerline length
             (setq cl-len (vlax-curve-getdistatparam
                            cl-ent
                            (vlax-curve-getendparam cl-ent)
                          )
                   cl-len (- cl-len (* collar 2.0))
                   steps  (/ cl-len ribWidth)
             )
             (if (= (logand (fix steps) 1) 1) ; T = odd
               (setq steps (fix steps))
               (setq steps (1+ (fix steps)))
             )
             (setq ribWidth (/ (- cl-len 0.25) (1- steps))
                   dist     collar ;0.125 ; distance along center line
             )

            
             (setq ribFlag 0
                   cflag   t
                   cnt     0
                   pl1     nil
                   pl3     nil
                   errflag nil
             )

             ;;  ----------   Create Rib End Points   -----------
             (repeat steps
               (setq pt (vlax-curve-getpointatdist cl-ent dist))
               (setq
                 curDer (trans
                          (vlax-curve-getfirstderiv
                            cl-ent
                            (vlax-curve-getparamatpoint cl-ent pt)
                          )
                          0
                          1
                        )
               )
               ;; Get angle 90 deg to curve
               (setq curAng (+ (/ pi 2) (angle '(0 0) curDer)))
               (setq ribRadius (if (zerop ribFlag)
                                 (/ RibShort 2)
                                 (/ RibLong 2)
                               )
               )
               (setq pt (trans pt 0 1)) ; WCS > UCS
               (setq p1 (polar pt curAng ribRadius))
               (setq p2 (polar pt (+ pi curAng) ribRadius))
               (if cflag ; create start collar points
                 (setq RibPtLst1 (list (polar p1 (angle curDer '(0 0)) collar))
                       RibPtLst2 (list (polar p2 (angle curDer '(0 0)) collar))
                       cflag     nil
                 )
               )

               ;;  this collection method creates a woven pline
               (cond
                 ((null pl1) ; first time through
                  (setq RibPtLst1 (cons p1 RibPtLst1)
                        RibPtLst2 (cons p2 RibPtLst2)
                  )
                 )
                 ((= (logand (setq cnt (1+ cnt)) 1) 1) ; T = odd cnt
                  (setq RibPtLst1 (cons pl2 RibPtLst1)
                        RibPtLst1 (cons p2 RibPtLst1)
                        RibPtLst2 (cons pl1 RibPtLst2)
                        RibPtLst2 (cons p1 RibPtLst2)
                  )
                 )
                 ((setq RibPtLst1 (cons pl1 RibPtLst1)
                        RibPtLst1 (cons p1 RibPtLst1)
                        RibPtLst2 (cons pl2 RibPtLst2)
                        RibPtLst2 (cons p2 RibPtLst2)
                  )
                 )
               )
               (if (and pl3 (inters p1 p2 pl3 pl4 t))
                 (setq errflag t)
               )
               (setq ribFlag (- 1 ribFlag) ; toggle flag
                     dist    (+ ribWidth dist)
                     pl3     pl1
                     pl4     pl2
                     pl1     p1
                     pl2     p2
               )
             )
             ;;  create end collar points
             (setq RibPtLst1 (cons p2 RibPtLst1)
                   RibPtLst1 (cons (polar p2 (angle '(0 0) curDer) collar) RibPtLst1)
                   RibPtLst2 (cons p1 RibPtLst2)
                   RibPtLst2 (cons (polar p1 (angle '(0 0) curDer) collar) RibPtLst2)
             )

             ;;  --------   point list to WCS   ------------
             (setq RibPtLst1 (mapcar '(lambda (x) (trans x 1 0)) RibPtLst1))
             (setq RibPtLst2 (mapcar '(lambda (x) (trans x 1 0)) RibPtLst2))

             ;;  --------   create jacket plines   ------------
             (or space
                 (setq space
                        (if (zerop (vla-get-activespace doc))
                          (if (= (vla-get-mspace doc) :vlax-true)
                            (vla-get-modelspace doc) ; active VP
                            (vla-get-paperspace doc)
                          )
                          (vla-get-modelspace doc)
                        )
                 )
             )

             (cond
               ((and errflag
                     (progn
                       (initget "Yes No")
                       (= "No"
                          (cond
                            ((getkword "\nTurns too tight, Proceed? [Yes/No]<Yes>:")) 
                            ("Yes")))
                       )
                     )
                t ; skip the create & stay in loop
               )
               ((setq newpline (makePline space RibPtLst1))
                (vla-put-layer newpline Flexlayer)
                (if FlexColor
                  (vla-put-color newpline FlexColor)
                )
                ;;(vla-put-elevation newpline z)

                (setq newpline2 (makePline space RibPtLst2))
                (vla-put-layer newpline2 Flexlayer)
                (if FlexColor
                  (vla-put-color newpline2 FlexColor)
                )
                ;;(vla-put-elevation newpline z)
                
                (if DelCL (entdel cl-ent)) ; remove the centerline object
                (if GroupFlex
                  (progn
                    (setq ss (ssadd))
                    (ssadd (vlax-vla-object->ename newpline) ss)
                    (ssadd (vlax-vla-object->ename newpline2) ss)
                    (or DelCl (ssadd cl-ent ss))
		   (command "erase" cl-ent "")
                    (if (vl-cmdf "_.-group" "_create" "*" "" ss "")
                      (princ "\nGrouping Done")
                      (princ "\nError Grouping")
                    )
                  )
                )

               )
             ) ; cond
           )        ; progn
           (princ "\nError - Can not use that object, Try again.")
         )          ; endif
         t
        )
        (t (princ "\nMissed Try again."))
      )             ; cond stmt
    )               ; progn
  )                 ; while
  (vla-endundomark doc)
  (vlax-release-object space)
  (vlax-release-object doc)
  ;;-----------  E N D   O F   L I S P  ----------------------------
  (princ)
)
;(prompt "\nFlex Duct loaded, Enter FLEX to run.")
(princ)



































































































































































































































































