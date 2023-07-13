; Block Name Change ------------------------------------------------------------------------------------------------------------------

(defun @bn()(c:bn))
(defun c:bn(/ myerror os ss1 en1 blist obn nbn dcl_id fn fname)

  (vl-load-com)
  ;start --- Internal error handler -------------------
   (defun myerror(S)
   (if (/= s "Function cancelled")(princ (strcat "\nError:" s)))
   (setvar "osmode" os)
   (setq *error* olderr)(princ) )
   (setq olderr *error* *error* myerror)
  ;end----------------------------------------
   (setvar "cmdecho" 0)
   (create_dialog_bn)
   (setq dcl_id (load_dialog fname))

   (setq os (getvar "osmode"))(terpri)   
   (setq ss1 (entsel "\n이름을 변경할 블럭을 선택하시오. :"))
   (if (= ss1 nil)  ; Null 媛믪뿉 ????븳 ?삤瑜? 泥섎━ 猷⑦떞
      (progn 
          (prompt "\n블럭을 선택하여야 합니다.")
      )
      (progn
       (setq en1 (car ss1)) ;entity name
       (if (eq (cdr (assoc 0 (entget en1))) "INSERT")
           (progn
             (setq blist (assoc 2 (entget en1))) ; block name list

             (setq obn (cdr blist)) ;block name
             (if (not (new_dialog "temp" dcl_id))(exit))  ;; Dialog Display
             (set_tile "text_edit" obn)
             (set_tile "bnn" obn)
             (action_tile "text_edit" "(setq nbn $value)")
             (action_tile "accept" "(done_dialog 1)")
             (action_tile "next" "(done_dialog 2)")
             (action_tile "cancel" "(done_dialog)")


         (setq key(start_dialog))

  (if (= key 1)
            (if (/= nbn obn) 
             (progn
                (command "rename" "b" obn nbn)
                (prompt "\n블럭이름이 변경됨 : ")
                (prin1 obn) (prompt " -> ") (prin1 nbn)
             ))
          )
      );if end 
)

  (if (= key 2)
            (if (/= nbn obn)
             (progn
                (setq elist (entget en1))
                (setq ins (cdr (assoc 10 elist)))

                (command "explode" ss1)
                (setvar "osmode" 0)
                (command "_.block" nbn ins "P" "")
                (command "_.insert" nbn ins "" "" "")
                (setvar "osmode" os)
                (prin1 obn) (prompt " -> ") (prin1 nbn)
             ))
          )
      );if end 
)



   
   (unload_dialog dcl_id)
   (setvar "cmdecho" 1)
   (prin1)
)

(defun create_dialog_bn()
 (setq fname (vl-filename-mktemp "dcl.dcl"))
 (setq fn (open fname "w"))
 (write-line "temp
   : dialog {
	  label = \"Block Name Change\";
	  initial_focus = \"text_edit\";

: boxed_column {
     label = \"◁ Blck Name ▷\";
 : row { 
     : text {label=\"현재 : \";} 
          :text {label=\"Name\";
                key=\"bnn\";
                width = 30;
               }}

	 : edit_box {
		label = \"변경 : \";
		key = \"text_edit\";
		edit_limit = 30;
		allow_accept = true;
             }
}
 spacer_1;
: boxed_column {
     label = \"◁ Option ▷\";

      : row { 
        : button { alignment = centered;
          label = \"전체 변경\";
          key = \"accept\";
          height = 3;
          is_default = true;
        } 
}
      : row {
: button { 
  key = \"next\"; 
  label = \"해당 객체만 변경\";

  height = 3;

  } 
}
}

: boxed_column {
  : row {
        : button {
          label = \"&Cancel\";
          key = \"cancel\";
          height = 2;
          is_cancel = true;
        }}	 
      }

  : row { 
    : image { key = \"im\" ;
              color = 5 ;
              width = 1.5;
              fixed_width = true;} 
      : paragraph {
      : text { label = \"Designed and Created                             \";}
      : text { label = \"By 100cm24kg.tistory.com.\";}
     }}

   }
  " fn)
  (close fn) 
);
(princ)
