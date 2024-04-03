;******************************  round bar & 특정 모양의중량 구하기  *********************************
(defun c:wwt () 
  (setvar "cmdecho" 0)
  (setvar "blipmode" 0)
  (SETQ OS_M (Getvar "OSMODE"))
  (COMMAND "OSMODE" 0)
  (if (= ss nil) (setq ss 0))
  (setq ABD ss)

  (PRINC "\n 두께를 입력하세요 <")
  (PRINC ABD)
  (PRINC "> : ")
  (setq ss (getreal " "))

  (setq h (getreal "\n 비중을 입력하세요 [setting 철:7.85] : "))
  (IF (= H NIL) (SETQ H 7.85))
  (SETQ HH H)
  (if (= ss nil) (setq ss ABD))

  (setq p (getpoint "\n 면의 내부 한 포인트를 지정하세요 : "))

  ; (command "bpoly" p "")
  (command "-boundary" "A" "I" "N" "" "" p "")
  (command "area" "e" "l")
  (command "erase" "l" "")

  (setq aR   (getvar "area")
        PER  (getvar "perimeter")
        ds   (GETVAR "DIMSCALE")
        DS3  (* 3 DS)
        TTPP (/ (* ss hh aR) 1000000)
        aa   (/ aR 100)
        bb   (/ PER 10)
        DS5  (* 2 DS3)
        aa   (rtos aa)
        bb   (rtos bb)
        TTT  (rtos ttPP)
        cc   (strcat "Weight    : " (substr ttt 1 12) "kg")
        aaa  (strcat "Area      : " (substr aa 1 12) "cm2")
        bbb  (strcat "Perimeter : " (substr bb 1 12) "cm")
        W    (strcat "T" (rtos ss) " x " "Area" (rtos AR) " x " "(7.85/1000000)" "=" TTT "Kg")
  )
  (WHILE (setq p (getpoint "\n 빠질 면적의 임의의 점 선택 : ")) 
    ; (command "bpoly" p "")
    (command "-boundary" "A" "I" "N" "" "" p "")
    (command "area" "e" "l")
    (command "erase" "l" "")
    (setq aR   (getvar "area")
          tt   (/ (* ss hh aR) 1000000)
          TTPP (- TTPP TT)
    )
  )
  (SETQ TTT (rtos ttPP))

  (princ "\n 옵션 선택 (<y> <enter> 입력) (<c> 고치기)  : ")
  (setq z (strcase (getstring)))
  (if (or (= z "") (= z "Y")) 
    (progn 
      (setq pp (getpoint "\n Text 위치는?")
            p1 (polar pp (dtr 90) dS5)
            p2 (polar p1 (dtr 90) dS5)
            p3 (polar p2 (dtr 90) dS5)
      )
      (command "text" pp DS3 "" ttt "")
      (command "text" p1 DS3 "" bbb "")
      (command "text" p2 DS3 "" aaa "")
      (command "text" p3 DS3 "" w "")
      (princ "\n   [ Area : ")
      (princ aa)
      (princ "cm2 ]")
      (princ "   [ Perimeter : ")
      (princ bb)
      (princ "cm ]")
      (princ ttt)
    )
  )

  (if (or (= z "c") (= z "C")) 
    (progn 
      (prompt "\n 바꿀 문자 선택 : ")
      (setq a (ssget))
      (setq b (sslength a))
      (setq c 0)
      (while (<= 1 b) 
        (setq d (ssname a c))
        (if (eq (cdr (assoc 0 (entget d))) "TEXT") 
          (entmod (subst (cons 1 tTt) (assoc 1 (entget d)) (entget d)))
        )
        (setq b (- b 1))
        (setq c (+ c 1))
      )
    )
  )
  (PRINC)
  (COMMAND "OSMODE" OS_M)
)

;*********************** 결과값 출력 하는 정의문 ****************
(DEFUN INSERT-TEXT (TOT UNT / TOT Z A B C D TXTHEI PT HH H) 
  (SETQ TOT (RTOS TOT 2 3))
  (PRINC (STRCAT "\n 총 합계 : " TOT UNT))

  (INITGET "Y C")
  (SETQ Z (GETKWORD "\n 옵션 선택 (<Y> <ENTER> 입력) (<C> 고치기)  : "))
  (IF (= Z "C") 
    (PROGN 
      (PROMPT "\n 바꿀 문자 선택 : ")
      (SETQ A (SSGET '((0 . "TEXT,MTEXT"))))
      (SETQ B (SSLENGTH A))
      (SETQ C 0)
      (WHILE (<= 1 B) 
        (SETQ D (SSNAME A C))
        (ENTMOD (SUBST (CONS 1 TOT) (ASSOC 1 (ENTGET D)) (ENTGET D)))
        (SETQ B (- B 1))
        (SETQ C (+ C 1))
      )
    )
    (PROGN 
      (SETQ TXTHEI (GETVAR "DIMTXT"))
      (SETQ PT (GETPOINT "\nTEXT POINT: "))
      (SETQ HH (GETVAR "DIMSCALE"))
      (SETQ H (* TXTHEI HH))
      (COMMAND "TEXT" PT H 0 TOT)
    )
  )
  (PRINC)
)

;******************************** STYLE NEW *************************************************
(DEFUN STYLE-NEW (STY / STY-CH FONTS) 
  (SETQ STY-CH (TBLSEARCH "STYLE" STY))
  (COND 
    ((= STY "GHS") (SETQ FONTS "ROMANS.SHX, GHS.SHX"))
    ((= STY "ARIAL") (SETQ FONTS "ARIAL.TTF"))
    ((= STY "ROMANS") (SETQ FONTS "ROMANS.SHX"))
    (PRINC "\n 문자 STYLE 가 없습니다.")
  )

  (IF (= STY "GHS") (COMMAND "STYLE" STY FONTS 0 0.8 0 "N" "N"))
  (IF (/= STY "GHS") (COMMAND "STYLE" STY FONTS 0 0.8 0 "N" "N" "N"))
  (PRINC)
)

;******************************** TABLE LIST *************************************************
(DEFUN TABLE-LIST (TAB-TYP / TAB-TYP LAPP EX LSS N TEMP LL) 
  (SETQ LAPP (APPEND LAPP (LIST (CDR (ASSOC 2 (TBLNEXT TAB-TYP T))))))
  (WHILE (SETQ EX (TBLNEXT TAB-TYP)) 
    (SETQ LAPP (APPEND LAPP (LIST (CDR (ASSOC 2 EX)))))
  )

  (SETQ LSS  (LENGTH LAPP)
        N    0
        TEMP ""
  )
  (PRINC "\n")
  (REPEAT LSS 
    (SETQ LL   (NTH N LAPP)
          N    (+ N 1)
          TEMP (STRCAT TEMP " " LL) ;;입력 부분 초기화 하기 위해서 문자열 형식으로 만듬
    )
    (PRINC (STRCAT "  " LL " "))
  )
  TEMP
)

;*************R12LISP***********************************
;-------------------DEGREE TO RADIAN----------------
(DEFUN DTR (ANG) 
  (* PI (/ ANG 180.0))
)
;
;--------------------RADIAN TO DEGREE-----------------
(DEFUN RTD (ANG) 
  (/ (* ANG 180) PI)
)

;;;;';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(DEFUN *ERROR* (ST) 
  (TERPRI)
  (PRINC "\n ERROR : ")
  (PRINC ST) ;에러 메세지 출력
  (COMMAND "UCS" "W")
  (COMMAND "SNAPANG" 0)
  (COMMAND "-LAYER" "T" "*" "") ;레이어를 모두 보이게 함.
  (IF (= (GETVAR "OSMODE") 0) (COMMAND "OSMODE" 2093)) ; OSMODE 가 0 이면 항상 스냅잡게
  (PRINC)
)

