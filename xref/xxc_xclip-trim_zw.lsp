

;;;아키모아 일단밟아님이 만든 리습입니다.
;;; xclip된 블럭을 삽입된 형태로 분해함. (explode 및 trim이용)
;;;ZOOM 이전 기능 추가
;;;2015.02.26
;;;2015.02.27 vtenable 값 적용






(defun c:xxc(/ k xref-10 x-ent pl1 p-pl1 p-ptl entx x-ents x-ents2
               p2 p22 pl2 p-pl2 p-ptl2 x-ents3 x-ent3 i olds chm ix vten)
  (setq vten (getvar "vtenable"))
  (prompt "\n\t xclip된 블럭을 삽입된 형태로 분해함. (explode 및 trim이용) ")

  (vl-cmdf "Undo" "Be")
  ;(setvar "vtenable" 2)

  (setq *error* jin4error)
  (setq olds (getvar "Osmode"))
  (setq chm (getvar "Cmdecho"))
  (setvar "Osmode" 0)
  (setvar "Cmdecho" 0)

  (setq k T)
  (defun dxf (id lst)(cdr (assoc id lst)))

  ;; 블럭의 이름을 받아들임.
  (while k    
    (setq x-ent(entsel "\n\t Xref-Block Select ?:"))
    (if (= (dxf 0 (setq x-entl (entget (car x-ent)))) "INSERT")
      (setq k nil b_name (dxf 2 x-entl))
      (prompt "\t ??Select is Not Block?? Select again ")
    )
  )

  (setq xref-10 (dxf 10 x-entl))
;;;  ;; 블럭의 삽입점에 블럭의 이름을 기입.
;;;  (vl-cmdf "TEXT" xref-10 300 "" b_name)

  ;; 블럭의 경계선을 생성하여 포인트리스트를 받아들임.
  (vl-cmdf "XCLIP" x-ent "" "P")
  (setq p-pl1 (entget (setq pl1 (entlast))))
  (setq p-ptmaxmin (pds_GetBoundingBox_ent pl1 'pl1-max 'pl1-min))
  (setq p-ptl (getpolyvtx p-pl1))
  (setq p-ptl (append p-ptl (list (nth 0 p-ptl))))

  (prompt "\n\t block-explode point")
  ;; 블럭을 분해하여 전체개체를 선택함.(이후에 erase및 trim을 하기위함)
  (setq entx (entlast))
  (vl-cmdf "explode" x-ent)

  ;; 분해한 블럭중에 경계선에 걸친 블럭이 있으면 분해함.
  (setq x-ents3 (ssget "F" p-ptl))
  (setq i 0)
  (repeat (sslength x-ents3)
    (setq x-ent3 (ssname x-ents3 i))
    (if (= "INSERT" (dxf 0 (entget x-ent3)))
      (if (and (= (dxf 70 (entget x-ent3)) 0)(= (dxf 71 (entget x-ent3)) 0)) ; 속성없는 블럭인지 확인.
        (vl-cmdf "Explode" x-ent3)
      )
    )
    (setq i (1+ i))
  )

  ;; 최종으로 분해하여 생긴 개체들을 선택함.
  (setq x-ents (ssadd) ix 1)
  (while (setq ent (entnext entx))
    (if (/= (dxf 0 (entget ent)) "SEQEND")
      (progn
        (setq x-ents (ssadd ent x-ents))
        (setq entx ent)
      )
;      (setq entx (entnext (dxf -1 (entget ent))))
      (setq entx ent)
    )
    (setq ix (1+ ix))
  )

  (prompt "\n\t select outside entty")
  ;; 경계선의 바깥에 존재하는 개체를 선택하여 지움.
  (setq x-ents2 (ssget "CP" p-ptl))
  (setq i 0)
  (repeat (sslength x-ents2)
    (ssdel (ssname x-ents2 i) x-ents)
    (setq i (1+ i))
  )
  (vl-cmdf "erase" x-ents "")

  (prompt "\n\t first trim")
  ;; 경계선의 바깥점 한곳을 선택해서 경계선을 옵셋하고 포인트리스트를 받아
  ;; 트림할 개체를 선택함.
  (setq p2  (polar pl1-max (angle pl1-min pl1-max)(* (distance pl1-max pl1-min) 0.01)))
  (setq p22 (polar pl1-min (angle pl1-max pl1-min)(* (distance pl1-max pl1-min) 0.01)))
  ;(setq p2 (getpoint "\n\t Box의 바깥쪽 한지점을 선택하시오. ?:"))
  (vl-cmdf "offset" "T" pl1 p2 "")
  (vl-cmdf "_.zoom" p2 p22)
  (setq p-pl2 (entget (setq pl2 (entlast))))
  (setq p-ptl2 (getpolyvtx p-pl2))
  (setq p-ptl2 (append p-ptl2 (list (nth 0 p-ptl2))))
  (vl-cmdf "erase" pl2 "") ; 옵셋된 경계선 삭제
  (repeat 5
    (vl-cmdf "Trim" pl1 "" "F")
    (mapcar 'vl-cmdf p-ptl2)
    (vl-cmdf "" "" )
  )

  (setvar "Osmode" olds)
  (setvar "Cmdecho" chm)
  (setvar "vtenable" vten)
  (command "zoom" "p")
  (vl-cmdf "Undo" "End")
)

(defun GetPolyVtx(EntList)
  (setq VtxList '())
  (foreach x EntList
   (if (= (car x) 10)
    (setq VtxList (append VtxList (list (cdr x))))
   )
  )
VtxList
)

(defun pds_GetBoundingBox_ent (ent pt1 pt2 / ent0 ptlst b-ent ps pe ptl pt max-x max-y min-x min-y maxp minp) ; sub
  (setq ptlst nil)
  (setq ent0 (dxf 0 (setq entl (entget ent))))
  (cond
    ((= ent0 "LINE")      (setq ps (dxf 10 entl) pe (dxf 11 entl) ptlst (append ptlst (list ps pe))))
    ((= ent0 "LWPOLYLINE")(setq ptl (GetpolyVtx entl) ptlst (append ptlst ptl)))
    ((= ent0 "POINT")     (setq pt (dxf 10 entl) ptlst (append ptlst (list pt))))
    (T nil)
  )
  (setq max-x (apply 'max (mapcar '(lambda (x) (car x)) ptlst))) ; x좌표중큰값
  (setq max-y (apply 'max (mapcar '(lambda (x) (cadr x)) ptlst))) ; y좌표중큰값
  (setq min-x (apply 'min (mapcar '(lambda (x) (car x)) ptlst))) ; x좌표중작은값
  (setq min-y (apply 'min (mapcar '(lambda (x) (cadr x)) ptlst))) ; x좌표중작은값

  (setq maxp (list max-x max-y) minp (list min-x min-y))
  (set (eval 'pt1) maxp)
  (set (eval 'pt2) minp)

 
)

;;; END OF XP5
