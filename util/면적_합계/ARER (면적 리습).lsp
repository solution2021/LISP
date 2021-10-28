(defun c:ARER (/ Sel EntData PolyObj TextObj ReactList Pos)
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

(vl-load-com)
(if
 (and
  (setq Sel (entsel "\n Select polyline to get area of: "))
  (setq EntData (entget (car Sel)))
  (= (cdr (assoc 0 EntData)) "LWPOLYLINE")
  (setq PolyObj (vlax-ename->vla-object (car Sel)))
  (setq Sel (entsel "\n Select text of hold area value: "))
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
(setq SqFt (/ (vla-get-Area PolyObj) 1000000))
(vla-put-TextString TextObj (rtos SqFt 2 2))
;;(setq SqFt (/ (vla-get-Area PolyObj) 144.0))
;;(vla-put-TextString TextObj (strcat (rtos SqFt 2 2) " SQ.FT."))
xRec
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