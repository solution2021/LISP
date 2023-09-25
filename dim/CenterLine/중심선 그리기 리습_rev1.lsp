(defun c:sq ( / olderr clay dimsc a b c d x ) ; 가로 센터라인 생성
    (setq olderr  *error*  *error* error)
    (setvar "cmdecho" 0)
  (if (= (tblsearch "LAYER" "3_중심선") nil)
     (command "layer" "new" "3_중심선" "color" "1" "3_중심선" "ltype" "center2" "3_중심선" "")
  )
  (setq cla (getvar "clayer"))
  (setq dimsc (getvar "dimscale"))
  (setq os (getvar "osmode"))
  (defun *error* (msg)(princ "error: ")(princ msg)
    (setvar "osmode" os)
    (setvar "clayer" cla)
    (setvar "dimscale" dimsc)
    (princ));-<*error* end
  ;|
  (setvar "osmode" 18)
  (setq a (getpoint "\n Enter first horizontal point: ")
        b (getpoint a "\n Enter opposite horizontal point: "))
  (setq c (list (/ (+ (car a) (car b)) 2.0) (/ (+ (cadr a) (cadr b)) 2.0))
        x (abs (- (car a) (car c)))
        d (* dimsc 0))
  (setvar "blipmode" 0)
  (setvar "osmode" 0) 
  (command "LINE" 
           (list (- (car c) (+ x d)) (cadr c))
           (list (+ (car c) (+ x d)) (cadr c))
           ""
  )
  (command "change" "l" "" "p" "la" "3_중심선" "")
  |;
  (HDrawCenterLine 5)
  
  (setvar "cmdecho" 1)
  (setvar "blipmode" 0)
  (setvar "osmode" 1975)
  (prin1)
)
;

(defun c:sa ( / olderr clay dimsc a b c d y ) ; 세로 센터라인 생성
    (setq olderr  *error*  *error* error)
  (if (= (tblsearch "LAYER" "3_중심선") nil)
     (command "layer" "new" "3_중심선" "color" "1" "3_중심선" "ltype" "center2" "3_중심선" "")
  )
  (setvar "cmdecho" 0)
  (setq cla (getvar "clayer"))
  (setq dimsc (getvar "dimscale"))
  (setq os (getvar "osmode"))
  (defun *error* (msg)(princ "error: ")
    (princ msg)
    (setvar "osmode" os)
    (setvar "clayer" cla)
    (setvar "dimscale" dimsc)
    (princ));-<*error* end
  ;|
  (setvar "osmode" 18)
  (setq a (getpoint "\n Enter first vertical point: ")
        b (getpoint a "\n Enter opposite vertical point: "))
  (setq c (list (/ (+ (car a) (car b)) 2.0) (/ (+ (cadr a) (cadr b)) 2.0))
        y (abs (- (cadr a) (cadr c)))
        d (* dimsc 0))
  (setvar "blipmode" 0)
  (setvar "osmode" 0)
  (command "LINE" 
           (list (car c) (- (cadr c) (+ y d)))
           (list (car c) (+ (cadr c) (+ y d)))
           ""
  )
  (command "change" "l" "" "p" "la" "3_중심선" "")
  |;
  (VDrawCenterLine 5)
  
  (setvar "cmdecho" 1)
  (setvar "blipmode" 0)
  (setvar "osmode" 1975)
  (prin1)
)
;

(defun c:s1 ( / olderr clay dimsc a b c d x ) ; 가로 세로 센터라인 생성 (사각)
  (setq olderr  *error*  *error* error)
  (if (= (tblsearch "LAYER" "3_중심선") nil)
     (command "layer" "new" "3_중심선" "color" "1" "3_중심선" "ltype" "center2" "3_중심선" "")
  )
  
  (setq cmd (getvar "cmdecho"))
  (setq cla (getvar "clayer"))
  (setq dimsc (getvar "dimscale"))
  (setq os (getvar "osmode"))
  
  (setvar "cmdecho" 0)
  
  (defun *error* (msg)
    (princ "error: ")
    (princ msg)
    (setvar "osmode" os)
    (setvar "clayer" cla)
    (setvar "dimscale" dimsc)
    (setvar "cmdecho" cmd)
    (princ));-<*error* end  
  
  ; 중심선 그리기
  (DrawCenterLine 5)
  
  ; 설정 되돌리기
  (setvar "cmdecho" cmd)
  (setvar "blipmode" 0)
  (setvar "clayer" cla)
  (setvar "dimscale" dimsc)
  (setvar "osmode" os)
  
  (prin1)
)

(defun DrawCenterLine(rate)
  ; 가로선
  (setvar "osmode" 18)
  (setq a (getpoint "\n Enter first horizontal point: ")
        b (getpoint a "\n Enter opposite horizontal point: "))
  (setq c (list (/ (+ (car a) (car b)) 2.0) (/ (+ (cadr a) (cadr b)) 2.0))
        x (abs (- (car a) (car c)))
        d rate)
  
  (setvar "blipmode" 0)
  (setvar "osmode" 0)
  (command "LINE" 
           (list (- (car c) (+ x d)) (cadr c))
           (list (+ (car c) (+ x d)) (cadr c))
           ""
  )
  (command "change" "l" "" "p" "la" "3_중심선" "")
  
  ; 세로선
  (setvar "osmode" 18)
  (setq a (getpoint "\n Enter first vertical point: ")
        b (getpoint a "\n Enter opposite vertical point: "))
  (setq c (list (/ (+ (car a) (car b)) 2.0) (/ (+ (cadr a) (cadr b)) 2.0))
        y (abs (- (cadr a) (cadr c)))
        d rate)
  
  (setvar "blipmode" 0)
  (setvar "osmode" 0)
  (command "LINE" 
           (list (car c) (- (cadr c) (+ y d)))
           (list (car c) (+ (cadr c) (+ y d)))
           ""
  )
  (command "change" "l" "" "p" "la" "3_중심선" "")
)

(defun HDrawCenterLine(rate)
  ; 가로선
  (setvar "osmode" 18)
  (setq a (getpoint "\n Enter first horizontal point: ")
        b (getpoint a "\n Enter opposite horizontal point: "))
  (setq c (list (/ (+ (car a) (car b)) 2.0) (/ (+ (cadr a) (cadr b)) 2.0))
        x (abs (- (car a) (car c)))
        d rate)
  
  (setvar "blipmode" 0)
  (setvar "osmode" 0)
  (command "LINE" 
           (list (- (car c) (+ x d)) (cadr c))
           (list (+ (car c) (+ x d)) (cadr c))
           ""
  )
  (command "change" "l" "" "p" "la" "3_중심선" "")
)

(defun VDrawCenterLine(rate)
  ; 세로선
  (setvar "osmode" 18)
  (setq a (getpoint "\n Enter first vertical point: ")
        b (getpoint a "\n Enter opposite vertical point: "))
  (setq c (list (/ (+ (car a) (car b)) 2.0) (/ (+ (cadr a) (cadr b)) 2.0))
        y (abs (- (cadr a) (cadr c)))
        d rate)
  
  (setvar "blipmode" 0)
  (setvar "osmode" 0)
  (command "LINE" 
           (list (car c) (- (cadr c) (+ y d)))
           (list (car c) (+ (cadr c) (+ y d)))
           ""
  )
  (command "change" "l" "" "p" "la" "3_중심선" "")
)