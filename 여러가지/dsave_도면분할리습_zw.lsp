; 아키모아 운영진 "행복한하루"
; http://cafe.daum.net/archimore
; 한파일안에 여러도면 찢기
; 2007.10.02. 
; 1차수정 2007.12.05 / 텍스트선택 "c" 로 변경 / 공백제거루틴변경 / 도면이름 2개이상일때 합쳐서만들기

(defun c:dsave(/ path os ent en minpt maxpt bn scal baseang p1 p2 p3 p4 dis1 dis2 dis3 dis4 ang1 ang2 ang3 ang4
  ss var tadd k en1 $p1 $p2 $p3 $p4 ss2 num svnum savename ss3 $$len $$k fname1 $$ed $$txt)
;->*error* start   
 (defun *error* (msg)(princ "error: ")(princ msg)
 (setvar "osmode" os) (if ent (redraw ent 4)) (command "ucs" "")
 (princ))
;-<*error* end
 (prompt "\n>> 한파일안에 여러도면 개별 저장하기..")
 (vl-load-com)
 (setvar "cmdecho" 0) 
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
   (command "zoom" "w" minpt maxpt)
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
   (setq ss (@ss_new_lst_x ss))
   (if ss
    (progn
     (setq var (getint "\n A-<1> / S-<2> / E-<3> / EF-<4> / M-<5> / MF-<6> / NONE<엔터> :"))
     (cond ((= var 1) (setq tadd "A-"))
           ((= var 2) (setq tadd "S-"))
           ((= var 3) (setq tadd "E-"))
           ((= var 4) (setq tadd "EF-"))
           ((= var 5) (setq tadd "M-"))
           ((= var 6) (setq tadd "MF-"))
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
         (setq ss1 (@ss_new_lst_x ss1))
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
          (setq num (@tspace-delete (cdr (assoc 1 (entget (ssname ss2 0))))))
         )
         (setq num "")
        )
        (setq svnum 0)
        (setq savename (strcat path tadd num "" fname))
        (while (findfile (strcat savename ".dwg"))
         (setq savename (strcat savename"_" (rtos (setq svnum (1+ svnum)) 2 0)))
        )
        (setq savename (strcat savename ".dwg"))
        (setq ss3 (ssget "w" minpt maxpt))
        (command "ucs" "m" minpt)
        (command "wblock" savename "" (list 0 0 0) ss3 "")
        (command "oops")
        (command "ucs" "")
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



(defun @ss_new_lst_x (ss / ssn n ss-y1 en en1y ss-y2 ss-y3) ;선택정렬
   (setq ssn (sslength ss))
   (setq n 0)
   (setq ss-y1 '())
   (repeat ssn
       (setq en (ssname ss n))
       (setq en1y (list en (cadr (cdr (assoc 10 (entget en)) ) ) ))
       (setq ss-y1 (cons en1y ss-y1))
       (setq n (+ n 1))
   )
   (setq ss-y2 (vl-sort ss-y1 '(lambda (e1 e2) (> (cadr e1) (cadr e2))))  )
   (setq n 0 ss-y3 (ssadd))
   (repeat ssn
      (setq ss-y3 (ssadd (car (nth n ss-y2)) ss-y3))
      (setq n (+ n 1))
   )
ss-y3
)