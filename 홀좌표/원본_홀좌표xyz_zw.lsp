;  ->OCS 좌표 인식
;;------ HoleTable (밀핀좌표) ------------------------------------------
(defun c:htb(/ title sca sslen th tx sp txp lsp lep 1p 2p 3p 4p 5p 6p
                  org xorg yorg zorg lp ep elist spt cp x y z rr ss xsp ysp zsp rsp
                  os bl ss ssen k dlf rr1)
   (setq os (getvar "osmode") bl (getvar "blipmode") dlf (getvar "dimlfac") uorg (getvar "ucsorg"))
   (prompt " 밀핀 선택<원,호만 선택됨>...")
   (setq ss (ssget))
   (setq org (getpoint "\n원점선택-> "))
   (setq sp  (getpoint "\n표 그리기 시작점(왼쪽상단)->  "))
   (setq tx  (getstring "\n첨자<E>: "))
   (if (= tx "")(setq tx "E"))
   (setq num (getint "\n첨자의 시작번호<1>: "))
   (if (= num nil)(setq num 1))
   (setq dot (getint "\n소수이하 자릿수<1>: "))
   (if (= dot nil)(setq dot 1))
;---<레이어 만들기(txt,1,5)
   (if (tblsearch "layer" "txt") ;->문자레이어
       (prompt " 문자")(command "layer" "n" "txt" "c" "white" "txt" "") )
   (if (tblsearch "layer" "1") ;->외곽선
       (prompt " 외곽선")(command "layer" "n" "1" "c" "red" "1" "") )
   (if (tblsearch "layer" "5") ;->내부선
       (prompt " 내부선")(command "layer" "n" "5" "c" "blue" "5" "") )
   (command "color" "bylayer")(terpri)
;--->
   (setvar "osmode" 0)(setvar "blipmode" 0)
   (setq   title "EJECTOR PIN POSITION"
           xorg (polar org 0 100) yorg (polar org (/ pi 2) 100)
           txp (list (+ (car sp) 5) (- (cadr sp) 9))
           lsp (list (car sp) (- (cadr sp) 5))
           lep (polar lsp 0 95)
           1p  (polar lsp 0 15) 2p (polar 1p 0 20)
           3p  (polar 2p 0 20)  4p (polar 3p 0 20)
           sca 1 th 2 lp lsp ep lep
   )
   (command "layer" "s" "1" "")
   (command "line" sp "@95,0" "")
   (command "line" lp ep "")
   (command "layer" "s" "txt" "")
   (command "text" "MC" (list (+ (car txp) 42.5) (+ (cadr txp) 6.5)) th 0 title)
   (command "text" "MC" (list (+ (car txp) 2.5) (+ (cadr txp) 1.5)) th 0 "No")
   (command "text" "MC" (list (+ (car txp) 20) (+ (cadr txp) 1.5)) th 0 "X")
   (command "text" "MC" (list (+ (car txp) 40) (+ (cadr txp) 1.5)) th 0 "Y")
   (command "text" "MC" (list (+ (car txp) 60) (+ (cadr txp) 1.5)) th 0 "Z")
   (command "text" "MC" (list (+ (car txp) 80) (+ (cadr txp) 1.5)) th 0 "Dia.")

;---<홀크기순 정렬
   (setq ss (@ss_en_2 ss "ARC" "CIRCLE"))
   (setq ssen (@en_r_lst ss))
   (setq sslen (length ssen))
   (setq k 0) (setq oldrr 100)
   (repeat sslen
     (setq elist (entget (nth k ssen)))
     (setq nor (cdr (assoc 210 elist)))	; Extrusion or Normal.(벡터)
     (setq pt (cdr (assoc 10 elist)))
     (setq cp (trans pt nor 1)) 		;circle point 홀중심
     
     (command "ucs" "3" org xorg yorg)	;원점을 org로 변경
     
     (setq  x  (rtos (/ (car   (trans cp 0 1)) sca) 2 dot)
            y  (rtos (/ (cadr  (trans cp 0 1)) sca) 2 dot)
            z  (rtos (/ (caddr (trans cp 0 1)) sca) 2 dot)
            rr (cdr  (assoc 40 elist))
            rr (/ (* rr 2) sca)
     )
     (setq newrr rr)
     (command "ucs" "P")
     (setq spt (polar (trans cp 0 1) (dtr 45) (+ (/ rr 2) 1.0)))
     (setq rr1 (* rr dlf));;-> dimlfac 적용
     (setq rr1 (strcat  "%%c" (rtos rr1 2 dot)) )
     (setq txno (strcat tx (itoa num)))
     (setq num (+ num 1))
     (command "layer" "s" "txt" "")
     (command "text" spt th 0 txno) ;홀에 부호 쓰기

;;--<표 작성 시작
      (setq lsp (polar lsp (dtr 270) 5)
            lep (polar lsp 0 95)
            txp (polar txp (dtr 270) 5)
            xsp (polar txp 0 28)
            ysp (polar txp 0 48)
            zsp (polar txp 0 68)
            rsp (polar txp 0 88)
      )
      (command "text" txp th 0 txno) ;부호쓰기
;;---- dimlfac 적용 시작 -----------------------
      (setq x (* (atof x) dlf) x (rtos x 2 dot))
      (setq y (* (atof y) dlf) y (rtos y 2 dot))
      (setq z (* (atof z) dlf) z (rtos z 2 dot))
;;---- dimlfac 적용 끝 -------------------------
      (command "text" "r" xsp th 0 x) ;x좌표
      (command "text" "r" ysp th 0 y) ;y좌표
      (command "text" "r" zsp th 0 z) ;z좌표
      (if (= oldrr newrr)
         (progn
             (command "layer" "s" "5" "")
             (setq lep (polar lsp 0 75))
             (command "line" lsp lep "") ;짧은라인
         )
         (progn
            (command "text" "r" rsp th 0 rr1) ;파이쓰기
            (command "layer" "s" "1" "")
            (command "line" lsp lep "") ;긴라인
      ))
      (command "layer" "s" "txt" "")
      (setq oldrr newrr)
      (setq k (+ k 1))
   );repeat end

   (command "layer" "s" "1" "")
   (setq lsp (polar lsp (dtr 270) 5)
          lep (polar lsp 0 95)
          5p  (polar lsp 0 15)
          6p  (polar lsp 0 35)
          7p  (polar lsp 0 55)
          8p  (polar lsp 0 75)
   )
   (command "line" lsp lep "")
   (command "line" (list (car lp) (+ (cadr lp) 5)) lsp "")
   (command "line" (list (car ep) (+ (cadr ep) 5)) lep "")
   (command "line" 1p 5p "")
   (command "line" 2p 6p "")
   (command "line" 3p 7p "")
   (command "line" 4p 8p "")
   (setvar "osmode" os) (setvar "blipmode" bl)
(prompt "\n①홀크기순 / ②X-좌표순 / ③Y-좌표순 으로 정렬됨...")
(prin1)
)
(defun dtr (a) (* pi (/ a 180.0)))

;;================================================================
;   SORT한(반지름/x-좌표/y-좌표) 리스트구하기(2007.4.주말농부)
;   ->결과:ename list
;;----entitiy name list-------------------------------------------
(defun @en_r_lst (ss / ss-en1 ss-en2 ss-en3 en k elist px py rad tem no)
   (setq ss-en1 '() ss-en2 '() k 0)
   (while (setq en (ssname ss k))
     (setq elist (entget en))
     (setq nor (cdr (assoc 210 elist)))	; Extrusion or Normal.(벡터)
     (setq pt (cdr (assoc 10 elist)))
     (setq cp (trans pt nor 1)) 		;circle point 홀중심
     (setq px (car		cp))
     (setq py (cadr		cp))
     (setq pz (caddr	cp))
     (setq rad (cdr (assoc 40 elist))) ;radius
     (setq tem (+ (+ (* px 100) py) (* rad 100000)))
     (setq ss-en1 (cons (list en tem) ss-en1))
     (setq k (+ k 1))
   )
   (setq ss-en2 (vl-sort ss-en1 '(lambda (x1 x2) (> (cadr x1) (cadr x2))))  )
   ;ss-en2 내림차순
   (setq ss-en3 '() k 0)
   (setq no (length ss-en2))
   (repeat no
       (setq ss-en3 (cons (car (nth k ss-en2)) ss-en3))
       (setq k (+ k 1))
   ) ;ss-en3가 radius 오름차순으로 변경됨(ename list)
ss-en3
)
;;=================================================================
;   명령어이름 두가지(arc,circle)가 포함된 리스트(2007.4.주말농부)
;   ->결과:선택세트<Selection set>
;;--- Selection set entity name (arc & circle) --------------------
(defun @ss_en_2 (ss te1 te2 / k en elist)
   (setq k 0)
   (while (setq en (ssname ss k))
      (setq elist (entget en))
      (if (or (= (cdr (assoc 0 elist)) te1)
              (= (cdr (assoc 0 elist)) te2) )
         (princ k)
         (progn (setq k (- k 1)) (ssdel en ss)   )
      )
      (setq k (+ k 1))
   )
ss
)
