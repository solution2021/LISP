; 아키모아 운영진 "행복한하루"
; http://cafe.daum.net/archimore
; 한파일안에 여러도면 찢기
; 2007.10.02. 
; 1차수정 2007.12.05 / 텍스트선택 "c" 로 변경 / 공백제거루틴변경 / 도면이름 2개이상일때 합쳐서만들기

(defun c:dsave(/ );path os ent en minpt maxpt bn scal baseang p1 p2 p3 p4 dis1 dis2 dis3 dis4 ang1 ang2 ang3 ang4
  ; ss var tadd k en1 $p1 $p2 $p3 $p4 ss2 svnum savename ss3 $$len $$k fname1 $$ed $$txt num)
;->*error* start   
 (defun *error* (msg)(princ "error: ")(princ msg)
 (setvar "osmode" os) (if ent (redraw ent 4)) (command "ucs" "")
 (princ))
;-<*error* end
 (setq vnum_index (getenv "WBLOCKEXSAVEASTYPEIDX"))
 (cond ((= vnum_index "0") (setq vnum "2018"))
	   ((= vnum_index "1") (setq vnum "2013"))
	   ((= vnum_index "2") (setq vnum "2010"))
	   ((= vnum_index "3") (setq vnum "2007"))
	   ((= vnum_index "4") (setq vnum "2004"))
	   ((= vnum_index "5") (setq vnum "2000"))
 )
	   
 (princ (strcat "현재 저장될 도면 버전 < " vnum " >")) 
 (initget 0 "1 2 3 4 5 6")
 (setq vnum_index1 (getkword "\n2018(1) 2013(2) 2010(3) 2007(4) 2004(5) 2000(6) 선택해주세요. (변경없을시 엔터)"))
 (cond ((= vnum_index1 "1") (setenv "WBLOCKEXSAVEASTYPEIDX" "0"))
	   ((= vnum_index1 "2") (setenv "WBLOCKEXSAVEASTYPEIDX" "1"))
	   ((= vnum_index1 "3") (setenv "WBLOCKEXSAVEASTYPEIDX" "2"))
	   ((= vnum_index1 "4") (setenv "WBLOCKEXSAVEASTYPEIDX" "3"))
	   ((= vnum_index1 "5") (setenv "WBLOCKEXSAVEASTYPEIDX" "4"))
	   ((= vnum_index1 "6") (setenv "WBLOCKEXSAVEASTYPEIDX" "5"))
	   ((= vnum_index1 nil) (setenv "WBLOCKEXSAVEASTYPEIDX" vnum_index))
 )
 (prompt "\n>> 한파일안에 여러도면 개별 저장하기..")
 (vl-load-com)
 (setq path (getvar "dwgprefix"))
 (setq os (getvar "osmode"))
 (setq ent nil)
 (while (= ent nil) (setq ent (car (entsel "\n블럭을 선택하세요. 한개만."))))
 (setq en (cdr (assoc 0 (entget ent))))
 (if (= en "INSERT")
  (progn
   (redraw ent 3)
   (setvar "osmode" 0)
   (vla-GetBoundingBox (vlax-ename->vla-object ent) 'MinPt 'MaxPt) 
   (setq MinPt (vlax-safearray->list MinPt)) 
   (setq MaxPt (vlax-safearray->list MaxPt))
   (command "zoom" minpt maxpt)
   (setq bn (cdr (assoc 2 (entget ent))))
   (setq scal (abs (cdr (assoc 41 (entget ent)))))
   (setq baseang (cdr (assoc 50 (entget ent)))) 
   (setq p1 (getpoint "\n도곽안에 저장될 파일명인 text 구역지정:")
         p2 (getcorner p1 " ->다음점:"))
   (setq dis1 (/ (distance minpt p1) scal) ang1 (angle minpt p1))
   (setq dis2 (/ (distance minpt p2) scal) ang2 (angle minpt p2))
   (setq p3 (getpoint "\n도면번호 text 구역지정-없으면 엔터:"))
   (if p3 
    (progn
     (setq p4 (getcorner p3 " ->다음점:"))
     (setq dis3 (/ (distance minpt p3) scal) ang3 (angle minpt p3))
     (setq dis4 (/ (distance minpt p4) scal) ang4 (angle minpt p4))
    )
   )
   (prompt "\n>> wblock으로 내보낼 sheet 선택:")
   (command "zoom" "e")
    
   (setq ss (ssget (list (cons 0 "insert") (cons 2 bn) (cons 50 baseang))))
   (setq ss ($TBSort ss 1))
   (if ss
    (progn
     (setq var (getint "\n EG-<1> / EE-<2> / ES-<3> / ET-<4> / EF-<5> / MF-<6> / EI-<7> / EO-<8> / NONE<엔터> :"))
     (cond ((= var 1) (setq tadd "EG-"))
           ((= var 2) (setq tadd "EE-"))
           ((= var 3) (setq tadd "ES-"))
           ((= var 4) (setq tadd "ET-"))
           ((= var 5) (setq tadd "EF-"))
           ((= var 6) (setq tadd "MF-"))
           ((= var 7) (setq tadd "EI-"))
           ((= var 8) (setq tadd "EO-"))
           (t (setq tadd ""))
     );cond 
     (setq k 0)
     (repeat (sslength ss)
      (setq en1 (ssname ss k))
      (setq scal (abs (cdr (assoc 41 (entget en1))))) 
      (vla-GetBoundingBox (vlax-ename->vla-object en1) 'MinPt 'MaxPt) 
      (setq MinPt (vlax-safearray->list MinPt)) 
      (setq MaxPt (vlax-safearray->list MaxPt))
      (setq $p1 (polar minpt ang1 (* dis1 scal))) 
      (setq $p2 (polar minpt ang2 (* dis2 scal))) 
      (setq ss1 (ssget "c" $p1 $p2 (list (cons 0 "text,mtext"))))
      (if ss1 
       (progn
;-----------------------------선택텍스트 합치기 추가
       (setq $$len (sslength ss1))
       (setq $$k 0 fname1 "")
       (if (> $$len 1)
        (progn
         (setq ss1 ($TBSort ss1 1))
         (repeat (sslength ss1)
          (setq $$ed (entget (ssname ss1 $$k)))
          (setq $$txt (cdr (assoc 1 $$ed)))
          (setq fname1 (strcat fname1 $$txt))
          (setq $$k (1+ $$k))
         ) 
        );progn
        (setq fname1 (cdr (assoc 1 (entget (ssname ss1 0)))))
       );if
;-----------------------------
        (setq fname (@tspace-delete fname1))
        (if p3 
         (progn
          (setq $p3 (polar minpt ang3 (* dis3 scal))) 
          (setq $p4 (polar minpt ang4 (* dis4 scal)))
          (setq ss2 (ssget "c" $p3 $p4 (list (cons 0 "text,mtext"))))
          (if ss2 
           (progn
;-----------------------------선택텍스트 합치기 추가
            (setq $$len (sslength ss2))
             (setq $$k 0 num1 "")
             (if (> $$len 1)
              (progn
               (setq ss2 ($TBSort ss2 1))
               (repeat (sslength ss2)
                (setq $$ed (entget (ssname ss2 $$k)))
                (setq $$txt (cdr (assoc 1 $$ed)))
                (setq num1 (strcat num1 $$txt))
                (setq $$k (1+ $$k))
               ) 
              );progn
              (setq num1 (cdr (assoc 1 (entget (ssname ss2 0)))))
             );if
;-----------------------------
         )
         (setq num "")
        )))
        (setq num (@tspace-delete num1))
        (setq svnum 0)
        (setq savename (strcat path tadd num "-" fname))
        (while (findfile (strcat savename ".dwg"))
         (setq savename (strcat savename "_" (rtos (setq svnum (1+ svnum)) 2 0)))
        )
        (setq ss3 (ssget "w" minpt maxpt))
        (command "ZOOM" "W" minpt maxpt)
        (command "wblock" savename "" minpt ss3 "")
        (command "oops")
        (command "ucs" "")
        (command "ZOOM" "P")
       )
       (progn (command "line" minpt maxpt "") (command "line" (list (car minpt) (cadr maxpt)) (list (car maxpt) (cadr minpt)) ""))
      ) 
      (setq k (1+ k))
     );repeat
;-----------------------------------------------------------------------------------
    )
   )
  )
 )
(setvar "osmode" os)
(princ)
);defun



(defun @tspace-delete(txt / k newascii @1 ascii_list) ; 공백제거
 (setq ascii_list (vl-string->list txt))
 (setq k 0 newascii '())
 (repeat (length ascii_list)
  (setq @1 (nth k ascii_list))
  (if (and (/= @1 32) (/= @1 92) (/= @1 47) (/= @1 58) (/= @1 63) (/= @1 60) (/= @1 62) (/= @1 34) (/= @1 42) (/= @1 124))  
   (setq newascii (append newascii (list @1))))
  (setq k (1+ k))
 )
 (setq newascii (vl-list->string newascii))
newascii
)





(defun $TBSort (sset m / VerLst Nset obj en inspt SLst n)
        (vl-load-com) 
        (setq VerLst '() Nset (ssadd) n 0) 
        (repeat (sslength sset) 
                (setq obj (vlax-ename->vla-object (setq en (ssname sset n)))) 
                (setq inspt (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint obj)))) 
                (setq VerLst (append VerLst (list (list en (fix (car inspt)) (fix (cadr inspt)))))) 
                (setq n (1+ n)) 
        ) 
        (cond ((= m 1) (setq SLst (vl-sort (vl-sort VerLst '(lambda (a b) (< (cadr a) (cadr b)))) '(lambda (a b) (> (caddr a) (caddr b)))))) 
                  ((= m 2) (setq SLst (vl-sort (vl-sort VerLst '(lambda (a b) (> (caddr a) (caddr b)))) '(lambda (a b) (< (cadr a) (cadr b)))))) 
        ) 
        (setq Nset (foreach x SLst (ssadd (car x) Nset))) 
)
