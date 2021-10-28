;;===============================================================
;  z값을 0으로 바꾸는 명령 (1996.9 주말농부)
;  ->진행과정 시각화(2006.11)
;;---------------------------------------------------------------
(defun c:32(/ ss ssnum cdw ptz en spt ept ptz sptx spty eptx epty)
   (prompt "\nCommand: 2D Entity...")
   (setq ss (ssget))
   (setq ssnum (sslength ss))
   (setq cdw 0.)
   (setq ptz 0)
   (repeat ssnum
       (setq en (ssname ss cdw))
       (setq spt (cdr (assoc 10 (entget en))))
       (if (/= spt nil)(progn
       (setq sptx (car spt) spty (cadr spt))
       (setq spt (list sptx spty ptz))
              (entmod (subst (cons 10 spt) (assoc 10 (entget en))(entget en)))
       ))
       (setq ept (cdr (assoc 11 (entget en))))
       (if (/= ept nil)(progn
       (setq eptx (car ept) epty (cadr ept))
       (setq ept (list eptx epty ptz))
              (entmod (subst (cons 11 ept) (assoc 11 (entget en))(entget en)))
       ))
       (setq elept (cdr (assoc 38 (entget en))))
       (if (/= elept nil)(progn
              (entmod (subst (cons 38 0) (assoc 38 (entget en))(entget en)))
       ))
       (setq cdw (1+ cdw))
       (princ (strcat "Coverting Data Num : " (rtos cdw 2 0) "(" (rtos (* (/ cdw ssnum) 100) 2 0) "%)" (chr 13)))
    )
    (prompt "...Converting End...")
    (princ)
)
