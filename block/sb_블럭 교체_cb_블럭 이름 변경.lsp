;;블록 바꾸는 리습

;(prompt "\nBlock Symbol Change >> cb [Enter] \n")
;(prompt "\nBlock Symbol Copy >> sb [Enter] \n")
;==============================================================================
(defun ech1 ()
(setq b (entget (ssname a k)))
(setq num (assoc c b))
(setq num8 (assoc 8 b))
(setq n-2 (cons 2 ns))
(setq n-8 (cons 8 ns))
(setq b (subst n-2 num b))
(setq b (subst n-8 num8 b))
(entmod b)
(setq k (+ k 1))
);defun ech1 end
;==============================================================================
(defun c:sb(/ fs fsn fsnl fs2 a n ns k) ;block symbol copy
(prompt "\nChange Block ")
(setq fs (entsel))
(setq fsn (car fs))
(setq fsnl (entget fsn))
(setq fs2 (cdr (assoc 2 fsnl)))
(setq a (ssget))
(setq n (sslength a))
(setq ns (strcase fs2))
(setq k 0 c 2)
(repeat n (ech1))
(prompt " Change Block < ") (princ ns) (prompt " >\n")
(setvar "cmdecho" 0)(princ)
);defun sb end
;==============================================================================
(defun c:cb(/ a n c k nb_name n2 n8 sname slist s0 s2 s8) ;block symbol change
(prompt "\nChange Block ")
(setq a (ssget))
(setq n (sslength a))
(setq c 2)
(setq k 0)
(setq nb_name (getstring "\n New Block Name : "))
; 블럭명 요소 만들기
(setq n2 (cons 2 nb_name))
(repeat n
  (setq sname (ssname a k))
  (setq slist (entget sname))
  ;(setq blockaccess (entget (cdr (assoc 330 slist))))
  (setq blockaccess (entget (cdr (assoc 330 (entget (tblobjname "block" (cdr (assoc 2 slist))))))))
  ; 블럭 검토
  (if (= (cdr(assoc 0 slist)) "INSERT")
    (progn
    ;(chn_blk s2 s8)
      ;(setq slist (subst n2 s2 slist))
      ;(entmod slist)
      (entmod (subst (cons 2 nb_name) (assoc 2 blockaccess) blockaccess))
    )
  )
  (setq k (1+ k)) 
)
(prompt (strcat " Change Block < " (itoa k) " >\n"))
;(prompt " Change Block < ") (princ k) (prompt " >\n")
(setvar "cmdecho" 0)(princ)
);defun cb end 