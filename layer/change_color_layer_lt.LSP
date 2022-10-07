(defun EntityChange (type name)
  (setq ent(ssget))
  (setq num(sslength ent))
  (setq i 0)
  (repeat num
    (setq sel(ssname ent i))
    (if (/= "LA" type)
      (progn
        (command "CHANGE" sel "" "P" type name "")
      )
      (progn
        (if (= nil (tblsearch "layer" name))
          (command "LAYER" "N" name ""))
        (command "CHANGE" sel "" "P" type name "")
      )
    )
    (setq i (+ i 1))
  )
)

;=================================================
; 자 료 명 : 색상변경
; 실 행 명 : 1/2/3/4/5/6/7
;====================================
;|
(defun C:1 () (command "SELECT" "\\" "CHANGE" "P" "" "P" "C" "7" "")) ;흰색
(defun C:2 () (command "SELECT" "\\" "CHANGE" "P" "" "P" "C" "8" "")) ;회색
(defun C:3 () (command "SELECT" "\\" "CHANGE" "P" "" "P" "C" "6" "")) ;선홍색
(defun C:4 () (command "SELECT" "\\" "CHANGE" "P" "" "P" "C" "1" "")) ;빨간색
(defun C:5 () (command "SELECT" "\\" "CHANGE" "P" "" "P" "C" "3" "")) ;녹색
(defun C:6 () (command "SELECT" "\\" "CHANGE" "P" "" "P" "C" "5" "")) ;파란색
(defun C:7 () (command "SELECT" "\\" "CHANGE" "P" "" "P" "C" "4" "")) ;하늘색
|;
(defun C:1 () (EntityChange "C" "7")) ;흰색
(defun C:2 () (EntityChange "C" "8")) ;회색
(defun C:3 () (EntityChange "C" "6")) ;선홍색
(defun C:4 () (EntityChange "C" "1")) ;빨간색
(defun C:5 () (EntityChange "C" "3")) ;녹색
(defun C:6 () (EntityChange "C" "5")) ;파란색
(defun C:7 () (EntityChange "C" "4")) ;하늘색

;=================================================
; 자 료 명 : 라인변경
; 실 행 명 : 9/0
;====================================
;|
(defun C:9 () (command "SELECT" "\\" "CHANGE" "P" "" "P" "LT" "CONTINUOUS" "")) ;실선 ─
(defun C:0 () (command "SELECT" "\\" "CHANGE" "P" "" "P" "LT" "HIDDEN" "")) ;숨은선 - - - - - -
|;
(defun C:9 () (EntityChange "LT" "CONTINUOUS")) ;실선 ─
(defun C:0 () (EntityChange "LT" "HIDDEN")) ;숨은선 - - - - - -

;=================================================
; 자 료 명 : 레이어변경
; 실 행 명 : 11/22/33/44/55/66
;====================================
;|
(defun C:11 () (command "SELECT" "\\" "CHANGE" "P" "" "P" "LA" "0"  "")) 
(defun C:22 () (command "SELECT" "\\" "CHANGE" "P" "" "P" "LA" "기기배치도"  "")) 
(defun C:33 () (command "SELECT" "\\" "CHANGE" "P" "" "P" "LA" "전원선포설도"  "")) 
(defun C:44 () (command "SELECT" "\\" "CHANGE" "P" "" "P" "LA" "접지선포설도"  "")) 
(defun C:55 () (command "SELECT" "\\" "CHANGE" "P" "" "P" "LA" "급전선포설도"  "")) 
(defun C:66 () (command "SELECT" "\\" "CHANGE" "P" "" "P" "LA" "안테나취부도"  "")) 
|;
(defun C:11 () (EntityChange "LA" "0"))
(defun C:22 () (EntityChange "LA" "기기배치도"))
(defun C:33 () (EntityChange "LA" "전원선포설도")) 
(defun C:44 () (EntityChange "LA" "접지선포설도")) 
(defun C:55 () (EntityChange "LA" "급전선포설도")) 
(defun C:66 () (EntityChange "LA" "안테나취부도")) 

;=================================================
; 자 료 명 : 레이어 전부켜기
; 실 행 명 : LOO
;====================================

(defun c:LOO (/ cmd lyr ss test)
     (setq cmd (getvar "cmdecho"))
     (setvar "cmdecho" 0)
     (command "layer" "on" "*" "")
     (setvar "cmdecho" cmd)
)

;=================================================
; 자 료 명 : 선택 레이어 만 켝기 (나머지 끄기)
; 실 행 명 : LO
;====================================

(defun C:LO (/ cmd ename elist lyr)
     (setq cmd (getvar "cmdecho"))
     (setvar "cmdecho" 0)
     (setq ename (car (entsel "\n Select an entity :")))
     (while (null ename)
	    (setq ename (car (entsel "\n Select again :")))
     )
     (setq elist (entget ename))
     (setq lyr (assoc 8 elist))
     (command "layer" "set" (cdr lyr) "")
     (command "layer" "off" "*" "" "")
     (command "layer" "on" lyr "" "")
     (prompt (strcat "\n Now the current layer is " (cdr lyr) "."))
     (setvar "cmdecho" cmd)
)

;=================================================
; 자 료 명 : 선택 레이어 끄기
; 실 행 명 : LP
;====================================
 
(defun c:LP (/ cmd)
     (setq cmd (getvar "cmdecho"))
     (setvar "cmdecho" 0)
     (setq ename (car (entsel "\n Select an entity :")))
     (while (null ename)
            (setq ename (car (entsel "\n Select again :")))
     )
     (while (or (null ename)
                (equal (cdr (assoc 8 (entget ename))) (getvar "clayer"))
            )
            (setq ename (car (entsel "\n Select another entity :")))
     )
    (setq elist (entget ename))
    (setq lyr (assoc 8 elist))
    (prompt (strcat "\n This layer is " (cdr lyr) "."))
    (setq test (getstring "\n Do you really want this layer off <N>:"))
    (if (or (equal test "Y") (equal test "y"))
         (progn (command "layer" "off" (cdr lyr) "")
           (prompt (strcat "\n Now the layer " (cdr lyr) " is off"))
         )
     )
     (setvar "cmdecho" cmd)
     (prin1)
)
(C:LFF)
