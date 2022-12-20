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
; �� �� �� : ���󺯰�
; �� �� �� : 1/2/3/4/5/6/7
;====================================
;|
(defun C:1 () (command "SELECT" "\\" "CHANGE" "P" "" "P" "C" "7" "")) ;���
(defun C:2 () (command "SELECT" "\\" "CHANGE" "P" "" "P" "C" "8" "")) ;ȸ��
(defun C:3 () (command "SELECT" "\\" "CHANGE" "P" "" "P" "C" "6" "")) ;��ȫ��
(defun C:4 () (command "SELECT" "\\" "CHANGE" "P" "" "P" "C" "1" "")) ;������
(defun C:5 () (command "SELECT" "\\" "CHANGE" "P" "" "P" "C" "3" "")) ;���
(defun C:6 () (command "SELECT" "\\" "CHANGE" "P" "" "P" "C" "5" "")) ;�Ķ���
(defun C:7 () (command "SELECT" "\\" "CHANGE" "P" "" "P" "C" "4" "")) ;�ϴû�
|;
(defun C:1 () (EntityChange "C" "7")) ;���
(defun C:2 () (EntityChange "C" "8")) ;ȸ��
(defun C:3 () (EntityChange "C" "6")) ;��ȫ��
(defun C:4 () (EntityChange "C" "1")) ;������
(defun C:5 () (EntityChange "C" "3")) ;���
(defun C:6 () (EntityChange "C" "5")) ;�Ķ���
(defun C:7 () (EntityChange "C" "4")) ;�ϴû�

;=================================================
; �� �� �� : ���κ���
; �� �� �� : 9/0
;====================================
;|
(defun C:9 () (command "SELECT" "\\" "CHANGE" "P" "" "P" "LT" "CONTINUOUS" "")) ;�Ǽ� ��
(defun C:0 () (command "SELECT" "\\" "CHANGE" "P" "" "P" "LT" "HIDDEN" "")) ;������ - - - - - -
|;
(defun C:9 () (EntityChange "LT" "CONTINUOUS")) ;�Ǽ� ��
(defun C:0 () (EntityChange "LT" "HIDDEN")) ;������ - - - - - -

;=================================================
; �� �� �� : ���̾��
; �� �� �� : 11/22/33/44/55/66
;====================================
;|
(defun C:11 () (command "SELECT" "\\" "CHANGE" "P" "" "P" "LA" "0"  "")) 
(defun C:22 () (command "SELECT" "\\" "CHANGE" "P" "" "P" "LA" "����ġ��"  "")) 
(defun C:33 () (command "SELECT" "\\" "CHANGE" "P" "" "P" "LA" "������������"  "")) 
(defun C:44 () (command "SELECT" "\\" "CHANGE" "P" "" "P" "LA" "������������"  "")) 
(defun C:55 () (command "SELECT" "\\" "CHANGE" "P" "" "P" "LA" "������������"  "")) 
(defun C:66 () (command "SELECT" "\\" "CHANGE" "P" "" "P" "LA" "���׳���ε�"  "")) 
|;
(defun C:11 () (EntityChange "LA" "0"))
(defun C:22 () (EntityChange "LA" "����ġ��"))
(defun C:33 () (EntityChange "LA" "������������")) 
(defun C:44 () (EntityChange "LA" "������������")) 
(defun C:55 () (EntityChange "LA" "������������")) 
(defun C:66 () (EntityChange "LA" "���׳���ε�")) 

;=================================================
; �� �� �� : ���̾� �����ѱ�
; �� �� �� : LOO
;====================================

(defun c:LOO (/ cmd lyr ss test)
     (setq cmd (getvar "cmdecho"))
     (setvar "cmdecho" 0)
     (command "layer" "on" "*" "")
     (setvar "cmdecho" cmd)
)

;=================================================
; �� �� �� : ���� ���̾� �� �U�� (������ ����)
; �� �� �� : LO
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
; �� �� �� : ���� ���̾� ����
; �� �� �� : LP
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