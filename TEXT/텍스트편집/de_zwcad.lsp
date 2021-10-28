
(defun c:DE (/ ddedits_error  ddedits_main  ddedits_dialog
                    ddedits_text ddedits_mtext ddedits_dim ddedits_att
                    ChangeSelected ChangeCase ChangeDefault
                    ReadText ReadHistory WriteHistory AddHistory
                    AddFirst AddFirst2 AddFirst3 AddFirst4
                    ReplaceText AddLast rtn load_dcl load_acadapp 
                    his0 his1 his2 his3 his4 his5 his6 his7 his8 his9
                    ReplaceAll  SelectedText NewText DefaultText
                    sset   ssl   nsset  temp sslen  unctr olderr  s   )

  (defun ddedits_error (s)
    (if HistoryFile (close HistoryFile))
    (if OpenFile (close OpenFile))
    (setq *error* olderr)
    (princ)
  )

  (defun ddedits_main ()
    (ReadHistory)
    (while (> sslen 0)
      (setq temp (ssname sset (setq sslen (1- sslen))))
      (cond
        ((= (cdr(assoc 0 (entget temp))) "TEXT" )    (ddedits_text ))
        ((= (cdr(assoc 0 (entget temp))) "MTEXT")    (ddedits_mtext))
        ((= (cdr(assoc 0 (entget temp))) "DIMENSION")(ddedits_dim))
        ((= (cdr(assoc 0 (entget temp))) "ATTDEF")   (ddedits_att))
      )
    )
    (WriteHistory)
  )

  (defun ddedits_text ()
    (setq ent (entget(ssname sset sslen)))
    (redraw (cdr (assoc -1 ent)) 3)
    (setq NewText (cdr (assoc 1 ent)))
    (if SelectedText
      (setq NewText SelectedText SelectedText nil)
      (setq DefaultText NewText)
    )
    (if (null ReplaceAll) (ddedits_dialog))
    (redraw (cdr (assoc -1 ent)) 1)
    (AddHistory (setq NewText (if ReplaceAll ReplaceAll NewText)))
    (if (and (= what_next 1) (> (strlen NewText) 0))
        (entmod (subst (cons 1 NewText) (assoc 1 ent) ent))
    )
    (if SelectedText (ChangeSelected))
  )

  (defun ddedits_mtext ()
    (if (= (getvar "platform") "386 DOS Extender")(ddedits_text)
        (progn
          (setq ent (entget(ssname sset sslen)))
          (redraw (cdr (assoc -1 ent)) 3)
          (command "_.DDEDIT" (ssname sset sslen) "")
          (redraw (cdr (assoc -1 ent)) 1)
        )
    )
  )

  (defun ddedits_dim ()
    (setq ent (entget(ssname sset sslen)))
    (redraw (cdr (assoc -1 ent)) 3)
    (setq NewText (cdr (assoc 1 ent)))
    (if SelectedText
      (setq NewText SelectedText SelectedText nil)
      (setq DefaultText NewText)
    )
    (if (null ReplaceAll) (ddedits_dialog))
    (redraw (cdr (assoc -1 ent)) 1)
    (AddHistory (setq NewText (if ReplaceAll ReplaceAll NewText)))
    (if (= what_next 1)(entmod (subst (cons 1 NewText) (assoc 1 ent) ent)))
    (if SelectedText (ChangeSelected))
  )

  (defun ddedits_att ()
    (setq ent (entget(ssname sset sslen)))
    (redraw (cdr (assoc -1 ent)) 3)
    (command "_.DDEDIT" (ssname sset sslen) "")
    (redraw (cdr (assoc -1 ent)) 1)
  )

  (defun ddedits_dialog ()
    (setq SelectedText nil)
    (if (not (new_dialog "ddedits" dcl_id))(exit))
    (set_tile "edt_txt" NewText)
    (set_tile "dsp_ver1" "Version 2.0 98/06/15                ")
    (set_tile "dsp_ver2" "Programed by Oh Gue Chul. edit by Han Sang Geun 2000/11 ")
    (set_tile "dsp_ver2" "Programed by Oh Gue Chul. edit by Han Sang Geun 2000/11. edit by hidraw 2010/4")
    

    (end_list)

    (setq chg_lst (append
      (list his0 his1 his2 his3 his4 his5 his6 his7 his8 his9)
      (ReadText "DDEDITS.CHG")
    ))
    (start_list "chg_txt")
    (mapcar 'add_list chg_lst )
    (end_list)

    (start_list "add_lst")
    (mapcar 'add_list (setq lst_txt (ReadText "DDEDITS.LST")))
    (end_list)

    (action_tile "edt_txt" "(setq NewText $value)")
    (action_tile "add_lst" "(AddLast (atoi $value))")
    (action_tile "chg_txt" "(ReplaceText (atoi $value))")
    (action_tile "chg_all" "(setq ReplaceAll (get_tile \"edt_txt\"))(done_dialog 1)")
    (action_tile "chg_sel" "(setq SelectedText (get_tile \"edt_txt\"))(done_dialog 1)")
    (action_tile "upper"   "(ChangeCase nil)")
    (action_tile "lower"   "(ChangeCase T  )")
    (action_tile "cancel"  "(done_dialog 0)")
    (action_tile "default" "(ChangeDefault)")

    (setq what_next (start_dialog))
  )

  (defun ChangeSelected ( / temp )
    (setq NewText SelectedText)
    (setq sslen (1+ sslen))
    (setq sed (entsel  "\nSelect new text: "))
    (if sed
      (if (= (cdr (assoc 0 (entget (car sed)))) "TEXT")
          (setq SelectedText (cdr (assoc 1 (entget (car sed)))))
      )
    )
  )

  (defun ChangeCase ( a )
    (set_tile "edt_txt" (setq NewText (strcase (get_tile "edt_txt") a)))
  )

  (defun ChangeDefault ()
    (setq NewText DefaultText)
    (set_tile "edt_txt" NewText)
  )

  (defun ReplaceText ( value )
    (setq NewText (nth value chg_lst))
    (set_tile "edt_txt" NewText)
  )

  (defun AddLast ( value )
    (setq NewText (nth value lst_txt))
    (set_tile "edt_txt" NewText)
  )

  (defun ReadText ( ReadFile / OpenFile TextList Readed)
    (if (findfile ReadFile)
      (progn
        (setq OpenFile (open (findfile ReadFile) "r"))
        (while (setq Readed (read-line OpenFile))
          (if TextList
            (setq TextList (append TextList (list Readed)))
            (setq TextList (list Readed))
          )
        )
        (close OpenFile)
      )
      (setq TextList (list ""))
    )
    (rtn TextList)
  )

  (defun ReadHistory ( / HistoryFile )
    (setq his0 "" his1 "" his2 "" his3 "" his4 "" his5 "" his6 "" his7 "" his8 "" his9 "")
    (if (findfile "DDEDITS.HIS")
      (progn
        (setq HistoryFile (open (findfile "DDEDITS.HIS") "r"))
        (setq his0 (read-line HistoryFile)) (if (null his0)(setq his0 ""))
        (setq his1 (read-line HistoryFile)) (if (null his1)(setq his1 ""))
        (setq his2 (read-line HistoryFile)) (if (null his2)(setq his2 ""))
        (setq his3 (read-line HistoryFile)) (if (null his3)(setq his3 ""))
        (setq his4 (read-line HistoryFile)) (if (null his4)(setq his4 ""))
        (setq his5 (read-line HistoryFile)) (if (null his5)(setq his5 ""))
        (setq his6 (read-line HistoryFile)) (if (null his6)(setq his6 ""))
        (setq his7 (read-line HistoryFile)) (if (null his7)(setq his7 ""))
        (setq his8 (read-line HistoryFile)) (if (null his8)(setq his8 ""))
        (setq his9 (read-line HistoryFile)) (if (null his9)(setq his9 ""))
        (close HistoryFile)
      )
    )
  )

  (defun WriteHistory (  )
    (if (findfile "DDEDITS.HIS")
      (setq HistoryFile (open (findfile "DDEDITS.HIS") "w"))
      (setq HistoryFile (findfile "DDEDITS.LSP")
            HistoryFile (substr HistoryFile 1 (- (strlen HistoryFile) 3))
            HistoryFile (open (strcat HistoryFile "HIS") "w")
      )
    )
    (write-line his0 HistoryFile)
    (write-line his1 HistoryFile)
    (write-line his2 HistoryFile)
    (write-line his3 HistoryFile)
    (write-line his4 HistoryFile)
    (write-line his5 HistoryFile)
    (write-line his6 HistoryFile)
    (write-line his7 HistoryFile)
    (write-line his8 HistoryFile)
    (write-line his9 HistoryFile)
    (close HistoryFile)
  )

  (defun AddHistory ( NewHistory )
    (cond 
      ((= NewHistory his0)(setq his0 NewHistory))
      ((= NewHistory his1)(setq his1 his0   his0 NewHistory))
      ((= NewHistory his2)(setq his2 his1   his1 his0   his0 NewHistory))
      ((= NewHistory his3)
       (setq his3 his2   his2 his1   his1 his0   his0 NewHistory)
      )
      ((= NewHistory his4)
       (setq his4 his3   his3 his2   his2 his1   his1 his0   his0 NewHistory)
      )
      ((= NewHistory his5)
       (setq his5 his4   his4 his3   his3 his2   his2 his1   his1 his0
             his0 NewHistory
       )
      )
      ((= NewHistory his6)
       (setq his6 his5   his5 his4   his4 his3   his3 his2   his2 his1
             his1 his0   his0 NewHistory
       )
      )
      ((= NewHistory his7)
       (setq his7 his6   his6 his5   his5 his4   his4 his3   his3 his2
             his2 his1   his1 his0   his0 NewHistory
       )
      )
      ((= NewHistory his8)
       (setq his8 his7   his7 his6   his6 his5   his5 his4   his4 his3
             his3 his2   his2 his1   his1 his0   his0 NewHistory
       )
      )
      ((= NewHistory his9)
       (setq his9 his8   his8 his7   his7 his6   his6 his5   his5 his4
             his4 his3   his3 his2   his2 his1   his1 his0   his0 NewHistory
       )
      )
      (t (setq his9 his8   his8 his7   his7 his6   his6 his5   his5 his4
               his4 his3   his3 his2   his2 his1   his1 his0   his0 NewHistory
         )
      )
    )
  )

  (defun rtn (value) value)

  (defun load_dcl (dcl_file / dcl_handle)
    (cond
      ((rtn (cdr (assoc dcl_file ai_support))))     
      ((not (findfile (strcat dcl_file ".dcl")))
        (ai_alert
          (strcat
            "Can't locate dialog definition file " dcl_file
            ".dcl\n Check your support directory."))
        (rtn nil))
      
      ((or (not (setq dcl_handle (load_dialog dcl_file)))
           (> 1 dcl_handle))
        (ai_alert
          (strcat
            "Can't load dialog control file " dcl_file ".dcl"
            "\n Check your support directory."))
        (rtn nil))

      (t (setq ai_support (cons (cons dcl_file dcl_handle) ai_support))
        (rtn dcl_handle))
    )
  )

  (defun load_acadapp ( / acad_ver )
    (setq acad_ver (substr (getvar "ACADVER") 1 2))
    (if (= acad_ver "13") (load_acadapp13) (load_acadapp14))
  )

  (defun load_acadapp13 ( / fname)
    (setq fname 
      (cond 
        ((eq (getvar "platform") "Windows")          "ACADAPP.EXE")
        ((eq (getvar "platform") "386 DOS Extender") "ACADAPP.EXP")
        (t "acadapp")
      )
    )
    (cond
      (  (= (type acad_colordlg) 'EXSUBR))
      (  (not (findfile fname))
         (alert (strcat " Can't find " fname ".  "))
         (rtn nil))
      (  (eq "failed" (xload fname "failed"))
         (alert (strcat " Can't load " fname ".  "))
         (rtn nil))
      (t)
    )
  )

  (defun load_acadapp14 ( / fname)
     (setq fname "ACADAPP.ARX")
     (cond
        (  (= (type acad_colordlg) 'EXRXSUBR))
        (  (not (findfile fname))
           (ai_alert (strcat "Can't find " fname "."))
           (ai_return nil))
        (  (eq "failed" (arxload fname "failed"))
           (ai_alert (strcat "Can't load " fname "."))
           (ai_return nil))
       (t)
     )
  )
  
  ;;;
  ;;; Starting DDEDITS.LSP
  ;;;
  (setq  olderr *error* *error* ddedits_error)

  (setq sset (ssget))
  (if (null sset) 
    (progn
      (princ "\nERROR: Nothing selected.")
      (exit)
    )
  )
  
  (setq ssl   (sslength sset)
        nsset (ssadd))
  (while (> ssl 0)
    (setq temp (ssname sset (setq ssl (1- ssl))))
    (if (or (= (cdr(assoc 0 (entget temp))) "TEXT")
            (= (cdr(assoc 0 (entget temp))) "MTEXT")
            (= (cdr(assoc 0 (entget temp))) "DIMENSION")
            (= (cdr(assoc 0 (entget temp))) "ATTDEF")
        )
        (ssadd temp nsset)
    )
  )
  (setq ssl (sslength nsset)
        sset nsset
        unctr 0
  )
  (print ssl)(princ "text entities found. ")
  (setq sslen (sslength sset))
  
  (cond
    ((not 
       (if (zerop (logand (getvar "cmdactive") (+ 1 2 4 8) )) T 
         (progn (alert "This command may not be invoked transparently.") nil)
       )
    ))
;    (  (not (load_acadapp)))
    (  (not (setq dcl_id(load_dcl "ddedits"))))
    (t (ddedits_main))
  )

  (setq *error* olderr)
  (princ)
)
(princ "   DDEDITS loaded.")
(princ)

