; Next available MSG number is    20 
; MODULE_ID AI_UTILS_LSP_
;;;----------------------------------------------------------------------------
;;;
;;;    ai_utils.lsp
;;;    
;;;  Copyright 2018 Autodesk, Inc.  All rights reserved.
;;;
;;;  Use of this software is subject to the terms of the Autodesk license 
;;;  agreement provided at the time of installation or download, or which 
;;;  otherwise accompanies this software in either electronic or hard copy form.
;;;
;;;----------------------------------------------------------------------------


;;; (ai_abort <appname> [<error message>] )
;;;
;;; Displays critical error message in alert box and terminates all
;;; running applications.
;;;
;;; If <errmsg> is nil, no alert box or error message is displayed.
(defun ai_abort (app msg)
  (defun *error* (s)
    (if old_error (setq *error* old_error))
    (princ)
  )
  (if msg
    (alert (strcat " Application error: "
                   app " \n\n  " msg "  \n" ) )
  )
  (exit)
)


(defun ai_return (value) value)  ; Make act of returning value explicit

;;; Beep function conditional on user-preferred setting.
(defun ai_beep ( / f)
  (write-line "\007" (setq f (open "CON" "w")))
  (setq f (close f))
)

;;; (ai_alert <message> )
;;;
;;; Shell for (alert)
(defun ai_alert (msg)
  (if ai_beep? (ai_beep))
  (alert (strcat " " msg "  "))
)

;;;  (ai_acadapp)
;;;
;;;  Check to see if acadapp is loaded (and load if necessary).
;;;
;;;  If ACADAPP is not loaded, then display a message indicating
;;;  such in an alert box, and return NIL to the caller.  This
;;;  function does not generate an error condition, so if that is
;;;  appropriate, then the caller must test the result and generate
;;;  the error condition itself.
;;;
(defun ai_acadapp ( / fname)
  t
)

;;; (ai_table <table name> <bit> )
;;;
;;; Returns a list of items in the specified table.  The bit values have the
;;; following meaning:
;;;  0  List all items in the specified table.
;;;  1  Do not list Layer 0 and Linetype CONTINUOUS.
;;;  2  Do not list anonymous blocks or anonymous groups.
;;;         A check against the 70 flag for the following bit:
;;;                  1  anonymous block/group
;;;  4  Do not list externally dependant items.
;;;         A check against the 70 flag is made for any of the following 
;;;         bits, which add up to 48:
;;;                 16  externally dependant
;;;                 32  resolved external or dependant
;;;  8  Do not list Xrefs.
;;;         A check against the 70 flag for the following bit:
;;;                  4  external reference
;;;  16 Add BYBLOCK and BYLAYER items to list.
;;;
(defun ai_table (table_name bit / tbldata table_list just_name)
  (setq tbldata nil)
  (setq table_list '())
  (setq table_name (xstrcase table_name))
  (while (setq tbldata (tblnext table_name (not tbldata)))
    (setq just_name (cdr (assoc 2 tbldata)))
    (cond 
      ((= "" just_name))               ; Never return null Shape names.
      ((and (= 1 (logand bit 1))
            (or (and (= table_name "LAYER") (= just_name "0"))
                (and (= table_name "LTYPE")
                     (= just_name "CONTINUOUS")
                )
            )
      ))
      ((and (= 2 (logand bit 2))
            (= table_name "BLOCK")
            (= 1 (logand 1 (cdr (assoc 70 tbldata))))
      )) 
      ((and (= 4 (logand bit 4))
            ;; Check for Xref dependents only. 
            (zerop (logand 4 (cdr (assoc 70 tbldata)))) 
            (not (zerop (logand 48 (cdr (assoc 70 tbldata)))))
            
      ))
      ((and (= 8 (logand bit 8))
            (not (zerop (logand 4 (cdr (assoc 70 tbldata)))))
      ))
      ;; Vports tables can have similar names, only display one.
      ((member just_name table_list)
      )
      (T (setq table_list (cons just_name table_list)))
    )
  )
  (cond
    ((and (= 16 (logand bit 16))
          (= table_name "LTYPE") ) (setq table_list (cons "BYBLOCK" 
     (cons "BYLAYER" table_list))) ) 
    (t) 
  ) 
  (ai_return table_list) 
)

;;;
;;; (ai_strtrim <string> )
;;;
;;; Trims leading and trailing spaces from strings.
(defun ai_strtrim (s)
  (cond 
    ((/= (type s) 'str) nil)
    (t (ai_strltrim (ai_strrtrim s)))
  )
)
(defun ai_strltrim (s)
  (cond 
    ((eq s "") s)
    ((/= " " (substr s 1 1)) s)
    (t (ai_strltrim (substr s 2)))
  )
)
(defun ai_strrtrim (s)
  (cond 
    ((eq s "") s)
    ((/= " " (substr s (strlen s) 1)) s)
    (t (ai_strrtrim (substr s 1 (1- (strlen s)))))
  )
)

;;;
;;; Pass a number, an error message, and a range.  If the value is good, it is
;;; returned, else an error is displayed.  
;;;  Range values:
;;;                 0 - any numeric input OK
;;;                 1 - reject positive
;;;                 2 - reject negative
;;;                 4 - reject zero
;;;                 
(defun ai_num (value error_msg range / good_value)
  (cond
    ;; is it a number
    ((not (setq good_value (distof value)))
      (set_tile "error" error_msg)
      nil
    )
    ;; is it positive
    ((and (= 1 (logand 1 range))
       (= (abs good_value) good_value)
     )
      (set_tile "error" error_msg)
      nil
    )
    ;; is it zero
    ((and (= 2 (logand 2 range))
       (= 0.0 good_value)
     )
      (set_tile "error" error_msg)
      nil
    )
    ;; is it negative
    ((and (= 4 (logand 4 range))
       (/= (abs good_value) good_value)
     )
      (set_tile "error" error_msg)
      nil
    )
    (T good_value)
  )
)

;;;
;;; Pass an angle and an error message.  If good, the angle is returned else
;;; nil and an error message displayed.
;;;
(defun ai_angle(value error_msg / good_value)
  (cond
    ((and (setq good_value (angtof value))
     )
      (set_tile "error" "")
      (atof (angtos good_value))
    )
    (T (set_tile "error" error_msg) nil)
  )
)

;;;
;;;  Error routine.
;;;
(defun ai_error (s)              ; If an error (such as CTRL-C) occurs
  (if (not (member s '("Function cancelled" "console break")))
    (princ (strcat "\nError: " s))
  )
  (if undo_init (ai_undo_pop))              ; Deal with UNDO
  (if old_error (setq *error* old_error))   ; Restore old *error* handler
  (if old_cmd (setvar "cmdecho" old_cmd))   ; Restore cmdecho value
  (princ)
)

;;;
;;; Routines that check CMDACTIVE and post an alert if the calling routine
;;; should not be called in the current CMDACTIVE state.  The calling 
;;; routine calls (ai_trans) if it can be called transparently or 
;;; (ai_notrans) if it cannot.
;;;
;;;           1 - Ordinary command active.
;;;           2 - Ordinary and transparent command active.
;;;           4 - Script file active.
;;;           8 - Dialogue box active.
;;;
(defun ai_trans ()
  (if (zerop (logand (getvar "cmdactive") (+ 2 8) ))
    T
    (progn 
      (alert "This command may not be invoked transparently.")
      nil
    )
  )
)

(defun ai_transd ()
  (if (zerop (logand (getvar "cmdactive") (+ 2) ))
    T
    (progn 
      (alert "This command may not be invoked transparently.")
      nil
    )
  )
)

(defun ai_notrans ()
  (if (zerop (logand (getvar "cmdactive") (+ 1 2 8) ))
    T
    (progn 
      (alert "This command may not be invoked transparently.")
      nil
    )
  )
)

;;; (ai_aselect)
;;;
;;; Looks for a current selection set, and returns it if found,
;;; or throws user into interactive multiple object selection,
;;; and returns the resulting selection set if one was selected.
;;;
;;; Sets the value of ai_seltype to:
;;;
;;;    1 = resulting selection set was autoselected
;;;    2 = resulting selection set was prompted for.
(defun ai_aselect ( / ss)
  (cond
    ((and (eq 1 (logand 1 (getvar "pickfirst")))
                 (setq ss (ssget "_i")) )
       (setq ss (ai_ssget ss))  ;; only if ss exists.
       (setq ai_seltype 1)
       (ai_return ss)
    )
    ((setq ss (ssget))
      (if ss (setq ss (ai_ssget ss)))
      (setq ai_seltype 2)
      (ai_return ss)
    )
  )
)

;;; (ai_aselect1 <msg> )
;;;
;;; Looks for ONE autoselected entity, or throws the user into
;;; interactive entity selection (one entity, where a selection
;;; point is insignificant).  <msg> is the prompt generated if
;;; interactive selection is invoked.
;;;
;;; Sets the value of ai_seltype to:
;;;
;;;    1 = resulting entity was autoselected
;;;    2 = resulting entity was prompted for.
(defun ai_aselect1 (msg / ent)
  (cond
    ((and (eq 1 (logand 1 (getvar "pickfirst")))
                 (setq ent (ssget "_i"))
                 (eq 1 (sslength ent)))
      (setq ai_seltype 1)
      (if (ai_entity_locked (ssname ent 0) 1)
        (ai_return nil)
        (ai_return (ssname ent 0))
      )
    )
    ((setq ent (entsel msg))
      (if (ai_entity_locked (car ent) 1) (setq ent nil))
      (setq ai_seltype 2)
      (ai_return (car ent))
    )
  )
)


;;; (ai_autossget1 <msg> )
;;;
;;; Same as ai_aselect1, but allows SELECT options, e.g. "Previous",
;;; and returns the first entity in the selection set.
;;; Looks for ONE autoselected entity, or throws the user into
;;; interactive entity selection (for one entity, where a selection
;;; point is insignificant).  The <msg> argument is the prompt used if
;;; interactive selection is invoked.
;;;
;;; Sets the value of ai_seltype to:
;;;
;;;    1 = resulting entity was autoselected
;;;    2 = resulting entity was prompted for.
(defun ai_autossget1 (msg / ent ss)
  (cond
    ((and (eq 1 (logand 1 (getvar "pickfirst")))
               (setq ent (ssget "_i"))
               (eq 1 (sslength ent))
          )
      (setq ai_seltype 1)
      (if (ai_entity_locked (ssname ent 0) 1)
        (ai_return nil)
        (ai_return (ssname ent 0))
      )
    )
    (T                  
      (while (not ss)
        (princ msg)
        (command "_.SELECT" "_SI" "_AU" pause)  
        (if (and 
              (setq ss (ssget "_P"))           
              (eq 1 (sslength ss))
              (setq ss (ai_ssget ss)) ;; removed locked entities
            )
          (setq ent (ssname ss 0))
          (setq ss nil ent nil)
        )
        (setq ai_seltype 2)
        (ai_return ent)
      )
    )
  )
)


;;;
;;; A function that turns on UNDO so that some existing routines will work.
;;; Do not use with new routines as they should be designed to operate with
;;; any UNDO setting.
;;;
;;; Fiberless Operations Update:
;;; Migrated (command) usage to (command-s) in order to work more harmoniously
;;; in error handlers. WCA  May 2010
;;; 
(defun ai_undo_on ()
  (setq undo_setting (getvar "undoctl"))
  (cond
    ((= 2 (logand undo_setting 2))     ; Undo is one
      (command-s "_.undo" "_control" "_all" "_.undo" "_auto" "_off")
    )
    ((/= 1 (logand undo_setting 1))    ; Undo is disabled
      (command-s "_.undo" "_all" "_.undo" "_auto" "_off")
    )
  )
)

;;;
;;; Return UNDO to the initial setting.  Do not use with new routines as they 
;;; should be designed to operate with any UNDO setting.
;;;
(defun ai_undo_off ()
  (cond 
    ((/= 1 (logand undo_setting 1))
      (command-s "_.undo" "_control" "_none")
    )
    ((= 2 (logand undo_setting 2))
      (command-s "_.undo" "_control" "_one")
    )
  )
)

;;;
;;; UNDO handlers.  When UNDO ALL is enabled, Auto must be turned off and 
;;; GROUP and END added as needed. 
;;;
(defun ai_undo_push()
  (setq undo_init (getvar "undoctl"))
  (cond
    ((and (= 1 (logand undo_init 1))   ; enabled
          (/= 2 (logand undo_init 2))  ; not ONE (ie ALL is ON)
          (/= 8 (logand undo_init 8))   ; no GROUP active
     )
      (command "_.undo" "_group")
    )
    (T)
  )  
  ;; If Auto is ON, turn it off.
  (if (= 4 (logand 4 undo_init))
      (command "_.undo" "_auto" "_off")
  )
)

;;;
;;; Add an END to UNDO and return to initial state.
;;;
(defun ai_undo_pop()
  (cond 
    ((and (= 1 (logand undo_init 1))   ; enabled
          (/= 2 (logand undo_init 2))  ; not ONE (ie ALL is ON)
          (/= 8 (logand undo_init 8))   ; no GROUP active
     )
      (command "_.undo" "_end")
    )
    (T)
  )  
  ;; If it has been forced off, turn it back on.
  (if (= 4 (logand undo_init 4))
    (command "_.undo" "_auto" "_on")
  )  
)
;;;
;;; (get_dcl "FILTER")
;;;
;;; Checks for the existence of, and loads the specified .DCL file,
;;; or aborts with an appropriate error message, causing the initial
;;; load of the associated application's .LSP file to be aborted as
;;; well, disabling the application.
;;;
;;; If the load is successful, the handle of the .DCL file is then
;;; added to the ASSOCIATION LIST ai_support, which would have the
;;; following structure:
;;;
;;;
;;;   (("DCLFILE1" . 1) ("DCLFILE2" . 2)...)
;;;
;;; If result of (ai_dcl) is NIL, then .DCL file is not avalable,
;;; or cannot be loaded (the latter can result from a DCL audit).
;;;
;;; Applications that call (ai_dcl) should test its result, and
;;; terminate or abort if it is nil.  Normal termination rather
;;; than aborting with an error condition, is desirable if the
;;; application can be invoked transparently.
;;;
(defun ai_dcl (dcl_file / dcl_handle)
  (cond
    ;; If the specified .DCL is already loaded then
    ;; just return its handle to the caller.
    ((ai_return (cdr (assoc dcl_file ai_support))))

    ;; Otherwise, try to FIND the .DCL file, and display a
    ;; an appropriate message if it can't be located, and
    ;; return Nil to the caller:
    ((not (findfile (strcat dcl_file ".dcl")))
      (ai_alert
        (strcat
          "Can't locate dialog definition file " dcl_file
          ".dcl\n Check your support directory."))
      (ai_return nil)
    )
    ;; The file has been found.  Now try to load it.  If it
    ;; can't be succesfully loaded, then indicate such, and
    ;; abort the caller:
    ((or (not (setq dcl_handle (load_dialog dcl_file)))
         (> 1 dcl_handle))
      (ai_alert
        (strcat
          "Can't load dialog control file " dcl_file ".dcl"
          "\n Check your support directory."))
      (ai_return nil)
    )
    ;; Otherwise, the file has been loaded, so add it's handle
    ;; to the FILE->HANDLE association list AI_SUPPORT, and
    ;; return the handle to the caller:
    (t (setq ai_support (cons (cons dcl_file dcl_handle) ai_support))
      (ai_return dcl_handle)
    )
  )
)

;;; Enable/Disable the common fields depending on the selection set.
;;; Layer 1; Color 2; Linetype 4; Linetype Scale 8; Thickness 16;
;;;
;;; Used by DDCHPROP and DDMODIFY.
;;;
(defun ai_common_state (ss_ename / bit_value)
  (setq bit_value 0)
  (setq ss_ename (strcase ss_ename))
  (cond
    ( (member ss_ename '("ARC" "ATTDEF" "CIRCLE"  "LINE" "POINT"
                         "POLYLINE" "SHAPE" "SOLID" "TRACE" "TEXT" "XREF"))
      (setq bit_value (logior 1 2 4 8 16))
    )
    ( (member ss_ename '("3DFACE" "ELLIPSE" "BODY"
                         "REGION" "3DSOLID" "SPLINE"
                         "XLINE" "TOLERANCE" "LEADER" "RAY"))
      (setq bit_value (logior 1 2 4 8))
    )
	( (member ss_ename '("DIMENSION" "INSERT"))
      (setq bit_value (logior 1 2 4)) 
    )
    ( (member ss_ename '("VIEWPORT" "MTEXT"))
      (setq bit_value (logior 1 2))
    )
    ( (member ss_ename '("MLINE"))
      (setq bit_value (logior 1 8))
    )
    (T (setq bit_value (logior 1 2 4 8 16)) ; Enable all fields if unknown.
    )
  )
  bit_value                         ; Return bit value of fields.
)

;;;
;;;
;;; (ai_helpfile) returns an empty string.  Let the core code figure out 
;;; the default platform helpfile.
;;;
(defun ai_helpfile ( / platform)
  ""
)

;;;
;;; Returns val with the any trailing zeros beyond the current 
;;; setting of luprec removed.
;;; 
(defun ai_rtos(val / a b units old_dimzin)
  (setq units (getvar "lunits"))
  ;; No fiddling if units are Architectural or Fractional
  (if (or (= units 4) (= units 5))
    (rtos val)
    ;; Otherwise work off trailing zeros
    (progn
      (setq old_dimzin (getvar "dimzin"))
      ;; Turn off bit 8
      (setvar "dimzin" (logand old_dimzin (~ 8)))
      (setq a (rtos val))
      ;; Turn on bit 8
      (setvar "dimzin" (logior old_dimzin 8))
      (setq b (rtos val units 15))
      ;; Restore dimzin
      (setvar "dimzin" old_dimzin)
      ;; Fuzz factor used in equality check.
      (if (equal (distof a) (distof b) 0.000001) a b)
    )
  )
)

;;;
;;; Returns angle val with the any trailing zeros beyond the current 
;;; setting of luprec removed.
;;; 
(defun ai_angtos(val / a b old_dimzin)
  (setq old_dimzin (getvar "dimzin"))
  ;; Turn off bit 8
  (setvar "dimzin" (logand old_dimzin (~ 8)))
  (setq a (angtos val))
  ;; Turn on bit 8
  (setvar "dimzin" (logior old_dimzin 8))
  (setq b (angtos val (getvar "aunits") 15))
  ;; Restore dimzin
  (setvar "dimzin" old_dimzin)
  ;; Fuzz factor used in equality check. Reminder a & b are radians.
  (if (equal (angtof a) (angtof b) 0.00000001) a b)
)

;;;
;;; When passed a selection set, (ai_ssget) removes objects on locked 
;;; layers from the returned selection set.  Nil is returned if all objects
;;; in the selection set are locked.  
;;;
(defun ai_ssget(ss / start_size end_size a diff)
  (setq start_size (sslength ss))

  (setq a 0)
  (while (< a (sslength ss))
    (if (ai_entity_locked (ssname ss a) 0)
      (ssdel (ssname ss a) ss)
      (setq a (1+ a))  ; Only increment if non-deleted item.
    )
  )

  (setq end_size (sslength ss))

  (if (/= 0 (setq diff (- start_size end_size)))
    (princ (strcat "\n" (itoa diff) " objects(s) on a locked layer.")) 
  )
  (if (> (sslength ss) 0)
    ss   
    nil
  )
)

;;;
;;; Returns T if passed ename is on a locked layer. 
;;;
(defun ai_entity_locked (ename message)
  (if (= 4 (logand 4 (cdr (assoc 70 
                            (tblsearch "layer" (cdr (assoc 8 (entget ename))))
                          ))))
    (progn
      (if (= 1 message)
        (princ "\n1 object on a locked layer. ")
      )
      T
    )
    nil
  )
)

;;; Integers in AutoLISP are 32-bit values.  However, when integers
;;; are transferred between AutoLISP and AutoCAD, they are restricted
;;; to 16-bit values (+32767 to -32768).  (sslength) returns real
;;; numbers when the number of entities exceeds 32767, and subsequent
;;; use of a variable expected to contain an int that actually contains
;;; a real causes those functions to fail ((ssname) for instance.)
;;;
;;; This wrapper ensures that the returned number is an int.  LTK, 1.96
(defun ai_sslength (ss)
  (if ss
    (fix (sslength ss))
    0
  )
)

;;;
;;; Set CMDECHO without undo recording
;;;
;;; This is useful for LISP defined commands that use UNDO grouping
;;; so that a single undo group is recorded in the undo/redo history
;;; Otherwise, the setting of the CMDECHO value may result in an
;;; additional "Group of Commands" entry in the undo/redo history.
;;;
(defun ai_setCmdEcho ( newVal / _oldEnvVal)
  ; Only do it if the value is different than the current value
  (if (/= newVal (getvar "CMDECHO"))
    (progn
      (setq _oldEnvVal (getenv "acedChangeCmdEchoWithoutUndo"))
      ; If not set yet, use 0 for default
      (if (not _oldEnvVal)
          (setq _oldEnvVal "0"))
      (setenv "acedChangeCmdEchoWithoutUndo" "1")
      (setvar "cmdecho" newVal)
      (setenv "acedChangeCmdEchoWithoutUndo" _oldEnvVal)
    )
  )
)

;;;Clean loading of ai_utils.lsp
(princ)
;;;-----BEGIN-SIGNATURE-----
;;; SgcAADCCB0YGCSqGSIb3DQEHAqCCBzcwggczAgEBMQ8wDQYJKoZIhvcNAQELBQAw
;;; CwYJKoZIhvcNAQcBoIIE3jCCBNowggPCoAMCAQICEA5dK+WnG5bDemPmWVSBRBgw
;;; DQYJKoZIhvcNAQELBQAwgYQxCzAJBgNVBAYTAlVTMR0wGwYDVQQKExRTeW1hbnRl
;;; YyBDb3Jwb3JhdGlvbjEfMB0GA1UECxMWU3ltYW50ZWMgVHJ1c3QgTmV0d29yazE1
;;; MDMGA1UEAxMsU3ltYW50ZWMgQ2xhc3MgMyBTSEEyNTYgQ29kZSBTaWduaW5nIENB
;;; IC0gRzIwHhcNMTcwODA0MDAwMDAwWhcNMTgwODA0MjM1OTU5WjCBijELMAkGA1UE
;;; BhMCVVMxEzARBgNVBAgMCkNhbGlmb3JuaWExEzARBgNVBAcMClNhbiBSYWZhZWwx
;;; FzAVBgNVBAoMDkF1dG9kZXNrLCBJbmMuMR8wHQYDVQQLDBZEZXNpZ24gU29sdXRp
;;; b25zIEdyb3VwMRcwFQYDVQQDDA5BdXRvZGVzaywgSW5jLjCCASIwDQYJKoZIhvcN
;;; AQEBBQADggEPADCCAQoCggEBALPR50hy1FkrWOBmP+sGXfKWFUpFAKB9OLDlN3Uj
;;; 94WBLdHje+wsBav/AOL1Ben4qOa74PWpJHTJd8jph4MSGhKZE3oFNPyAVXCVhUAj
;;; qlLaYQXkHDWMeyz+y7FWX4oK1B1H+SNVcnc2+kAB0bEIT4VAIvQcyva41ThpVGzP
;;; XZM/JKDDpA6tocMQ3935UAjHYuvoOADEkFt5O/lEWzPTz0aQkVLGiD18rgFxuSw+
;;; Uz2jyuDZZ5lyNBQRF+K4cu8fle9uL2WqbaO7koHz76dkJrNW9wAmkdGCdfj3MQo+
;;; OD4O5JjSMYHEcmjVbHyo+ZK/BIVykApxc0tfN2HRJSuHlG0CAwEAAaOCAT4wggE6
;;; MAkGA1UdEwQCMAAwDgYDVR0PAQH/BAQDAgeAMBMGA1UdJQQMMAoGCCsGAQUFBwMD
;;; MGEGA1UdIARaMFgwVgYGZ4EMAQQBMEwwIwYIKwYBBQUHAgEWF2h0dHBzOi8vZC5z
;;; eW1jYi5jb20vY3BzMCUGCCsGAQUFBwICMBkMF2h0dHBzOi8vZC5zeW1jYi5jb20v
;;; cnBhMB8GA1UdIwQYMBaAFNTABiJJ6zlL3ZPiXKG4R3YJcgNYMCsGA1UdHwQkMCIw
;;; IKAeoByGGmh0dHA6Ly9yYi5zeW1jYi5jb20vcmIuY3JsMFcGCCsGAQUFBwEBBEsw
;;; STAfBggrBgEFBQcwAYYTaHR0cDovL3JiLnN5bWNkLmNvbTAmBggrBgEFBQcwAoYa
;;; aHR0cDovL3JiLnN5bWNiLmNvbS9yYi5jcnQwDQYJKoZIhvcNAQELBQADggEBALfg
;;; FRNU3/Np7SJ5TRs8s8tPnOTd4D5We+stLCuQ0I1kjVIyiIY+Z3cQz2AB9x8VXuYF
;;; LcXnT6Rc1cMYJtlTyB7Z7EZkfxQmFgc4chVfnguTpPqUtfo3QMT/S1+QIdYfIbk1
;;; dSvFBmZwRGatmGbn2h7HGiIgNqQaO6TD7Fx9TEJPwIiiCK8F3b4ENpYQHlgH3OAd
;;; CRLa1IWPfeA03yF3PIq8+NhLsngw1FNm9+C6UOM3mf3jHwxTrbt4ooIZstjPA4PU
;;; G16FkhJg7l2RCDR6sE9iT7FMCsO6tAHX3pS8afFyNyEVfgJVKfzohdDOj+XQLkzp
;;; c9v3Xoh1gTIPCte7VPsxggIsMIICKAIBATCBmTCBhDELMAkGA1UEBhMCVVMxHTAb
;;; BgNVBAoTFFN5bWFudGVjIENvcnBvcmF0aW9uMR8wHQYDVQQLExZTeW1hbnRlYyBU
;;; cnVzdCBOZXR3b3JrMTUwMwYDVQQDEyxTeW1hbnRlYyBDbGFzcyAzIFNIQTI1NiBD
;;; b2RlIFNpZ25pbmcgQ0EgLSBHMgIQDl0r5acblsN6Y+ZZVIFEGDANBgkqhkiG9w0B
;;; AQsFADANBgkqhkiG9w0BAQEFAASCAQCx3nxi82RJLSG+wmQwAg6nt1rMGfAdrddz
;;; +NY2Hn/YDh0hzri+0DJT7s55emHCXysR5rBIQuPsRNbXHmoKH+MvljY/3x9oq+/X
;;; VHaimsDePwablacbZ4sJ3v8DV7zqZP3qyxQjqDvB2rYIA4QLts48SjMq6Kh5t8oe
;;; gB1Qw1XJltaqoOAvP6XBE2Phams0YdUoL6C/bv9+4P4II7WkJL6eDlaBW4sCHU7N
;;; UHWjHuw6CxpKJpt40qJY80W10xM2CAVgxfruhmN716yZxRI2kvnqOKRmRHQNqSCC
;;; +CE8bU7c2/6t93czp9RemPkgcqkbzD3ad+qm4tJSqc4fqvu/kaU7oWUwYwYDVR0O
;;; MVwEWjQAMQA7ADEALwAzADAALwAyADAAMQA4AC8ANAAvADUAMQAvADQAMQAvAFQA
;;; aQBtAGUAIABmAHIAbwBtACAAdABoAGkAcwAgAGMAbwBtAHAAdQB0AGUAcgAAAA==
;;; -----END-SIGNATURE-----