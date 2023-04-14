
(defun *error* (msg)                    
      (setvar "osmode" old_osmode)
     (princ msg)
)

(defun c:cc (/ ent pt1 pt2 rad)
  (princ "\n원 또는 호 객체 선택 : ")
  (setq cir_sel (ssget '((0 . "CIRCLE,ARC"))))
  (setq len (sslength cir_sel))
  ;(setq ent (car (entsel "\nSelect circle: ")))
  (setq count 0)
  
  (while (< count len)
    (setq cir_ent (ssname cir_sel count))
    (setq ent_name (cdr (assoc 0 (entget cir_ent))))
    
    (setq Cenpt (cdr (assoc 10 (entget cir_ent))))     
    (setq rad 	(cdr (assoc 40 (entget cir_ent)))) 
    (setq ex_line (* rad 0.1))
    
    (setq pt1 (list (car Cenpt) (- (- (cadr Cenpt) rad)ex_line) 0.0))
    (setq pt2 (list (car Cenpt) (+ (+ (cadr Cenpt) rad)ex_line) 0.0))
    (setq pt3 (list (- (- (car Cenpt) rad)ex_line) (cadr Cenpt) 0.0))
    (setq pt4 (list (+ (+ (car Cenpt) rad)ex_line) (cadr Cenpt) 0.0))
    
    (if (/= (cdr (assoc 2 (tblsearch "ltype" "CENTER2"))) "CENTER2")
      (command "_linetype" "L" "CENTER2" "" "")
    )
    
    (setq old_osmode (getvar "osmode"))
    (setvar "osmode" 0)
    
    (command "_line" pt1 pt2 "")
    (command "_chprop" (entlast) "" "_ltype" "CENTER2" "")
    (command "_line" pt3 pt4 "")
    (command "_chprop" (entlast) "" "_ltype" "CENTER2" "")
    
    (setq count (1+ count))
  )
  (setvar "osmode" old_osmode)
  (princ)
)
