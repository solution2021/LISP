;;===============================================================
;  Text Arrange(2006.11.주말농부)
;  ->다중선택된 문자를 기준점을 정하여 좌우로 정렬하는 명령어
;  ->문자의 시작점이나 끝점을 맞추어 정렬할 때 사용
;  ->수정 보완(2008.8)
;;---------------------------------------------------------------
(defun c:ta(/ myerror os ss n ptx pty pyxy kw k en tx a)
  ;start --- Internal error handler -----------------------------
   (defun myerror(S)
   (if (/= s "Function cancelled")(princ (strcat "\nError:" s)))
   (setvar "osmode" os) (setvar "blipmode" bl)
   (setq *error* olderr)(princ) )
   (setq olderr *error* *error* myerror)
  ;end-----------------------------------------------------------
   (setq os (getvar "osmode"))
   (prompt " 문자 좌우 정렬하기...")
   (setq ss (ssget '((0 . "text")))) (terpri)
   (setq n (sslength ss))
   (setq ptx (car (getpoint "\n좌우정렬점 선택->")))(terpri)
   (setvar "osmode" 0)
   (setq a "자리맞추기 선택[왼쪽(L)/중심(C)/오른쪽(R)] <왼쪽>: ") 
   (initget "Left Center Right")
   (setq kw (getkword a))
   (if (= kw nil) (setq kw "Left"))
   (if (= kw "Left") (setq sn 10) (setq sn 11))
   (command "TJUST" ss "" kw) (terpri)
   (setq k 0)
   (while (<= 1 n)
      (setq en (ssname ss k))
      (setq ptxy (cdr (assoc 10 (entget en)))  )
      (setq pty (cadr ptxy))
      (setq pt2 (list ptx pty))
      (entmod (subst (cons sn pt2) (assoc sn (entget en))(entget en)))
      (setq n (- n 1))
      (setq k (+ k 1))
   )
   (setvar "osmode" os)
   (princ)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;===============================================================
;  문자 상하 정렬하기 (2006.12.주말농부)
;  ->다중선택된 문자의 상하 간격을 바꾸는 명령어(2008.7.11)
;  ->가장 상단의 문자를 기준으로 간격이 변경됨
;;------ text arrange -------------------------------------------
(defun c:taa(/ os bl ss n h1 pt1 pt2 d1 pty pt1x k en)
;->*error* start
 (defun *error* (msg)(princ "error: ")(princ msg)
 (setvar "osmode" os)(setvar "blipmode" bl)
 (princ))
;-<*error* end
   (setq os (getvar "osmode") bl (getvar "blipmode"))
   (prompt " 문자 상하간격 바꾸기...")
   (setq ss (ssget '((0 . "text")))) (terpri)
   (setq n (sslength ss))
   (setvar "blipmode" bl)
   (setq h1 (cdr (assoc 40 (entget (ssname ss 0)))))
   (if (= d2 nil) (setq d2 h1))
   (princ (strcat "문자간격 입력<" (rtos d2 2 3) ">:"))
   (setq d1 (getdist))
   (if (= d1 nil) (setq d1 d2))
   (prompt "\n문자간격 -> ") (prin1 d1) (prompt "으로 변경")
   (setq d2 d1)
   (setvar "osmode" 0)
   (setq ss (@taa_lst ss))
   (setq pt (dxf 10 (entget (ssname ss 0))))
   (setq pty (cadr pt))
   (setq k 1)
   (repeat (- n 1)
      (setq en (ssname ss k))
      (setq ed (entget en))
      (setq pt1 (dxf 10 ed))
      (setq pt1x (car pt1))
      (setq pt2 (list pt1x (- pty d1)))
      (vl-cmdf "move" en "" pt1 pt2)
      (setq pty (- pty d1))
      (setq k (+ k 1))
   )
   (setvar "osmode" os)(setvar "blipmode" bl)
(prin1))
;;================================================================
;  새로운 선택세트 만들기(2008.7.11 주말농부)
;  ->y좌표가 큰순서로 선택세트을 재정렬하는 리습
;;----------------------------------------------------------------
(defun @taa_lst (ss / ssn n ss-y1 en en1y ss-y1 ss-y2 ss-y3 e1 e2)
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
;;;새로운 선택세트 만들기
   (setq n 0 ss-y3 (ssadd))
   (repeat ssn
      (setq ss-y3 (ssadd (car (nth n ss-y2)) ss-y3))
      (setq n (+ n 1))
   )
ss-y3)




