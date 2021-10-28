;;program name : 캐드내 사칙연산프로그램 ver 1.0
;;program : +, -, /, * 가지고 텍스트 계산 다 됨


(defun exe_1()
   (setvar "cmdecho" 0)
   (setvar "blipmode" 0)
   (setq e1 (entsel "\n>>첫번째숫자선택:"))
   (setq a (car e1);;----------엔티티의 이름값
         b (entget a);;--------엔티티의 리스트값
         txt1 (assoc 1 b);;----엔티티중 해당 문자열 리스트
         tt1 (cdr txt1);;------엔티티 문자열의 리스트중 뒤의값="문자열"
         sum1 (atof tt1);;-----엔티티의 문자열을 정수로 변환=100
         txth1 (assoc 40 b);;--엔티티 리스트중 텍스트 높이의 리스트값(40,100)
         thh (cdr txth1);;-----엔티티의 텍스트높이의 리스트중 뒤의값=100
   )          
   (setq e2 (entsel "\n>>두번째숫자선택:"))
   (setq aa (car e2)
         bb (entget aa)
         txt2 (assoc 1 bb)
         tt2 (cdr txt2)
         sum2 (atof tt2)
         )
    (setq tl (assoc 8 b)
          txtst (cdr tl)
    )
)

(defun exe+()
   (setq sum (+ sum1 sum2))
   (setq summ (rtos sum 2 3))
   (setq p1 (getpoint "\n>>표시할 포인트찍기:"))
   (command "text" p1 thh 0 summ)
     
 )
(defun exe-()
   (setq sum (- sum1 sum2))
   (setq summ (rtos sum 2 3))
   (setq p1 (getpoint "\n>>표시할 포인트찍기:"))
   (command "text" p1 thh 0 summ)
  
)

 
 (defun exe*()
    (setq sum (* sum1 sum2))
    (setq summ (rtos sum 2 3))
    (setq p1 (getpoint "\n>>표시할 포인트찍기:"))
    (command "text" p1 thh 0 summ)
 )

 (defun exe/()
    (setq sum (/ sum1 sum2))
    (setq summ (rtos sum 2 3))
    (setq p1 (getpoint "\n>>표시할 포인트찍기:"))
    (command "text" p1 thh 0 summ)
 )
 (defun exe%()
    (setq sum (* (/ sum1 sum2) 100))
    (setq summ (rtos sum 2 3))
    (setq p1 (getpoint "\n>>표시할 포인트찍기:"))
    (command "text" p1 thh 0 summ)
 )

(defun c:+( / e1 a b txt1 tt1 sum1 txth1 thh e2 aa bb txt2 tt2 sum2 tl txtst sum summ p1)
  (exe_1)
  (exe+)
  (princ)
)
(defun c:-( / e1 a b txt1 tt1 sum1 txth1 thh e2 aa bb txt2 tt2 sum2 tl txtst sum summ p1)
   (exe_1)
   (exe-)
   (princ)
)
(defun c:*( / e1 a b txt1 tt1 sum1 txth1 thh e2 aa bb txt2 tt2 sum2 tl txtst sum summ p1)
   (exe_1)
   (exe*)
   (princ)
)
(defun c:/( / e1 a b txt1 tt1 sum1 txth1 thh e2 aa bb txt2 tt2 sum2 tl txtst sum summ p1)
  (exe_1)
  (exe/)
  (princ)
)
(defun c:%( / e1 a b txt1 tt1 sum1 txth1 thh e2 aa bb txt2 tt2 sum2 tl txtst sum summ p1)
  (exe_1)
  (exe%)
  (princ)
)
(defun c:py( / e1 a b txt1 tt1 sum1 txth1 thh sum summ p1 )
   (setvar "cmdecho" 0)
   (setvar "blipmode" 0)
   (setq e1 (entsel "\n>>숫자선택:"))
   (setq a (car e1);;----------엔티티의 이름값
         b (entget a);;--------엔티티의 리스트값
         txt1 (assoc 1 b);;----엔티티중 해당 문자열 리스트
         tt1 (cdr txt1);;------엔티티 문자열의 리스트중 뒤의값="문자열"
         sum1 (atof tt1);;-----엔티티의 문자열을 정수로 변환=100
         txth1 (assoc 40 b);;--엔티티 리스트중 텍스트 높이의 리스트값(40,100)
         thh (cdr txth1);;-----엔티티의 텍스트높이의 리스트중 뒤의값=100
   ) 
    (setq sum (* sum1 0.3025))
    (setq summ (rtos sum 2 3))
    (setq zz (strcat "(" summ ")"))
    (setq p1 (getpoint "\n>>표시할 포인트찍기:"))
    (command "text" p1 thh 0 zz)
(princ)
)

(defun c:+( / sum_m ss ok e ent sum_ p1 txth1 thh summ ) 
 (setvar "cmdecho" 0)
 (setvar "blipmode" 0)
 (setq sum_m 0)
  (setq SS (ssget))
  (setq ok 0)
  (while 
    (setq e (ssname ss ok))
    (setq ent (entget e))
    (setq sum_ (assoc 1 ent))
    (setq sum_ (cdr sum_))
    (setq sum_ (atof sum_))
    (setq sum_m (+ sum_m sum_))
    (setq ok (1+ ok))
  )
 (setq p1 (getpoint "\n>>표시할 포인트찍기:"))
    (setq txth1 (assoc 40 ent))
    (setq thh (cdr txth1))
    (setq summ (rtos sum_m 2 3))
    (command "text" p1 thh 0 summ)
(princ)       
)

(defun c:*( / sum_m ss ok e ent sum_ p1 txth1 thh summ ) 
 (setvar "cmdecho" 0)
 (setvar "blipmode" 0)
 (setq sum_m 1)
  (setq SS (ssget))
  (setq ok 0)
  (while 
    (setq e (ssname ss ok))
    (setq ent (entget e))
    (setq sum_ (assoc 1 ent))
    (setq sum_ (cdr sum_))
    (setq sum_ (atof sum_))
    (setq sum_m (* sum_m sum_))
    (setq ok (1+ ok))
  )
 (setq p1 (getpoint "\n>>표시할 포인트찍기:"))
    (setq txth1 (assoc 40 ent))
    (setq thh (cdr txth1))
    (setq summ (rtos sum_m 2 3))
    (command "text" p1 thh 0 summ)
(princ)       
)
(princ "\n>>사칙연산 프로그램 로딩완료 command : +,-,*,/,%,py,++,**")
(princ)











































































































