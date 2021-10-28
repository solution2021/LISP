
;; RelativePaths by Joe Burke

;; The shortcut is REP.

;; Bug reports may be sent to me directly at lowercase@hawaii.rr.com.
;; Program updates will be posted at www.theswamp.org under 
;; "Show your stuff" in a topic named "Relative xref paths".

;; COMMAND LINE:

;; The following option is presented at the command line if the
;; active file contains both DWG and image references.
;; Otherwise, the routine runs without options.
;; Relative paths for [/Xrefs/Images/Both] <B>:

;; PROGRAM NOTES:

;; The primary intent is to change full paths to relative paths.
;; In some cases an existing relative path may be shortened.
;; Works with xref and image reference files.

;; The standard disclaimer applies. Use at your own risk...

;; In general it's important to understand how relative paths work
;; before using this program. Note, a relative path can only point to
;; a file on the same drive as the active file.

;; Reloading an xref can cause associative dimensions/leaders to do
;; strange things. Something to watch for. If it happens, undo
;; and change such dimensions to non-associative.

;; Unloaded images will be reloaded if the path is changed.

;; The program may take a long time depending on the number of references, 
;; how many are reloaded and the size of reloaded files.

;; Tested with 2002, 2004, 2006 and 2008. Should work with 2000,
;; but not tested.

;; Thanks to Steve Doman and Jason Piercey for their help.

;; VERSION HISTORY

;; Version 1.0 - 10/24/2009.
;; Combined RelativeXrefs.lsp and RelativeImages.lsp. into this routine
;; which does both.

;; Version 1.0a - 2/22/2010.
;; Revised the primary routine at end so the option to choose
;; Relative paths for [/Xrefs/Images/Both] <B>:
;; is only presented when the file contains image references.

;; Version 1.0b - 3/9/2011.
;; Revised the XrefSearch and ImageSearch functions.
;; Set the shortest possible relative path when an xref or image
;; is found one folder above or below the location of the active file.
;; Previous versions did not handle this situation correctly.
;; The relative path would typically work, but it was longer than necessary.
;; IOW, not a fatal bug, but a messy result. This version will fix the 
;; problem in a file where it occurred before using a previous version.

;; Version 2.0 - 6/17/2011.

;; This is a complete rewrite of the previous code which should be faster
;; and more faithful to the original concept of the code.

;; Added support for reference file types PDF, DWF and DGN for ACAD 
;; versions which support those file type references. Options to choose 
;; file type are presented at the command line when the active file 
;; contains references to those file types. Otherwise the routine processes 
;; xrefs without options at the command line.

;; An option is presented in the case of an invalid saved path and the 
;; reference is found in the same folder as the active file.
;; "Invalid saved path detected: <saved path>"
;; "Found at: <path to the active file>"
;; "Allow repath to the found at directory? [Yes/No] <Y>: "
;; This is intended to alert the user to the fact the invlaid saved
;; path will be lost in the process, something the routine does not
;; typically do.

;; Unkile previous versions, an unloaded image reference which is 
;; repathed is not reloaded.

;; There is a known issue with underlay references, DGN, DWF and PDF file
;; types. If unloaded and repathed, such a file will appear to be reloaded
;; on screen, when in fact it should not appear so. If the active file is 
;; saved, closed and reopened, the underlay reference will be unloaded.
;; No solution found yet for this problem.

;; The command line report presented when the routine ends is expanded to 
;; include a listing of references not found and those found on a drive 
;; other than the drive where the active file resides. A relative path
;; requires the active file and a reference must reside on the same drive.

;; The command line report also lists the number of nested image, DWF, DGN
;; and PDF file types found if any and depending on which version of ACAD is 
;; in use.

;; Thanks to Mark McDonough for some ideas borrowed from his xrp.lsp routine.
;; Thanks to Steve Doman for his comments and time spent testing this version.

(defun c:RelativePaths ( / *error* doc docfullpath blocks what ss nestedthings 
                            version FilePath IsRelativePath SSVLAList 
                            CompareAdjacentPaths PathListBelow Spinbar 
                            ComparePathLengths PathCount RelativeFilePosition 
                            RelativeImages RelativeXrefs NestedStuff)

  (defun *error* (msg)
    (cond
      ((not msg))
      ((wcmatch (strcase msg) "*QUIT*,*CANCEL*"))
      (T (princ (strcat "\nError: " msg)))
    )
    (graphscr)
    (vla-EndUndoMark doc)
    (princ)
  ) ;end error
  
  (vl-load-com)

  ;;;; Start functions common to both RelativeImages and RelativeXrefs ;;;;

  ;; Author unknown.
  (defun Spinbar (sbar)
    (cond ((= sbar "\\") "|")
          ((= sbar "|") "/")
          ((= sbar "/") "-")
          (t "\\")
    )
  ) ;end

  ;; Works when the parent reference is unloaded by
  ;; accessing the XrefDataBase of the parent reference.
  ;; Previous versions which just scanned the xref block did not 
  ;; work then the parent xref was unloded.
  ;; Returns a dotted pair list.
  ;; Example: ((5 . "Image") (1 . "DGN") (1 . "DWF") (0 . "PDF"))
  ;; Five nested images found, etc.
  (defun NestedStuff ( / name namelst objname imgcnt dgncnt dwfcnt 
                         pdfcnt db ms lst)

    (setq imgcnt 0 dgncnt 0 dwfcnt 0 pdfcnt 0)
    (vlax-for x blocks
      ;; Added 6/3/2011. NestedStuff is called before the main routine
      ;; displays any prompts. Calling spinbar fills an apparent pause
      ;; at the command line when NestedStuff takes a long time.
      (princ
        (strcat "\rExamining references...  " 
          (setq *sbar (Spinbar *sbar)) "  \t\t\t")
      )
      (if 
        (and 
          (= -1 (vlax-get x 'IsXref))
          (not (vl-catch-all-error-p 
            (setq db (vl-catch-all-apply 'vlax-get (list x 'XrefDataBase))))
          )
          (setq ms (vlax-get db 'Modelspace))
        )
        (vlax-for i ms
          (setq objname (vlax-get i 'ObjectName))
          (cond
            ((eq "AcDbRasterImage" objname)
              (setq name (vlax-get i 'Name))
              (if (not (vl-position name namelst))
                (setq namelst (cons name namelst)
                      imgcnt (1+ imgcnt)
                )
              )
            )
            ((eq "AcDbDgnReference" objname)
              (setq name (vlax-get i 'UnderlayName))
              (if (not (vl-position name namelst))
                (setq namelst (cons name namelst)
                      dgncnt (1+ dgncnt)
                )
              )
            )
            ((eq "AcDbDwfReference" objname)
              (setq name (vlax-get i 'UnderlayName))
              (if (not (vl-position name namelst))
                (setq namelst (cons name namelst)
                      dwfcnt (1+ dwfcnt)
                )
              )
            )
            ((eq "AcDbPdfReference" objname)
              (setq name (vlax-get i 'UnderlayName))
              (if (not (vl-position name namelst))
                (setq namelst (cons name namelst)
                      pdfcnt (1+ pdfcnt)
                )
              )
            )
          )
        )
      )
    )
    (setq lst (list (cons imgcnt "Image")
      (cons dgncnt "DGN") (cons dwfcnt "DWF") (cons pdfcnt "PDF"))
    )
    ;; Remove zero count items from list.
    (vl-remove-if '(lambda (x) (zerop (car x))) lst)
  ) ; end NestedStuff

  ;; Arguments:
  ;;   fn is a file name with extension - examples "abc.dwg" or "abc.jpg".
  ;;   curpath is the saved path.
  ;; Returns either the full saved path or a full path to file name if it is
  ;; found in the same folder as the active file. Otherwise nil if file name
  ;; is not found at either location.
  (defun FilePath (fn curpath / docpath res)
    (setq docpath (vlax-get (vla-get-ActiveDocument (vlax-get-acad-object)) 'Path))
    (cond
      ((setq res (findfile curpath)))
      ((setq res (findfile (strcat docpath "\\" fn))))
    )
    res
  ) ; end FilePath

  ;; Return T if the path passed is relative, otherwise nil.
  (defun IsRelativePath (path)
    (or
      (= 0 (vl-string-search ".\\" path))
      (= 0 (vl-string-search "..\\" path))
    )
  ) ;; end IsRelativePath

  ;; Argument: a selection set
  ;; Returns: a list of VLA objects
  (defun SSVLAList (ss / obj lst i)
    (setq i 0)
    (if ss
      (repeat (sslength ss)
        (setq obj (vlax-ename->vla-object (ssname ss i))
              lst (cons obj lst)
              i (1+ i)
        )
      )
    )
    (reverse lst)
  ) ;end SSVLAList

  ;; Called when the RelativeFilePosition function returns "adjacent"
  ;; which means the active file full path and a reference full path
  ;; do not share some common folder structure.
  ;; Returns: a list of folder names pointing to the reference path
  ;; and the length of doclst
  (defun CompareAdjacentPaths (docpath xrefpath / doclst xlst PathList)
    ;; Argument: full path string
    ;; Returns the path portion as a list of strings.
    ;; Example: (setq p "C:\\PROJECTS\\SAI Projects\\Plan 13 lv KMSPA.dwg")
    ;; Returns ("\\PROJECTS" "\\SAI Projects")
    (defun PathList (path / idx pat pos lst)
      ;; Remove the drive letter given a full path.
      (if (setq pos (vl-string-search ":" path))
        (setq path (substr path (+ 3 pos)))
      )
      (setq idx 0 pat "\\")
      (while (setq pos (vl-string-search pat path idx))
        (setq lst (cons (strcat pat (substr path (1+ idx) (- pos idx))) lst)
              idx (1+ pos)
        )
      )
      (reverse lst)
    ) ;end PathList

    (setq doclst (PathList docpath))
    (setq xlst (PathList xrefpath))
    (while 
      (and
        doclst
        xlst
        (eq (strcase (car doclst)) (strcase (car xlst)))
      )
      (setq doclst (cdr doclst))
      (setq xlst (cdr xlst))
    )
    (list xlst (length doclst))
  ) ; end CompareAdjacentPaths 

  ;; Called when the RelativeFilePosition function returns "below"
  ;; which means a reference was found at a folder location below the 
  ;; active file.
  (defun PathListBelow (path / pos lst)
    ;; Remove the drive letter given a full path.
    (if (setq pos (vl-string-search ":" path))
      (setq path (substr path (+ 2 pos)))
    )
    ;; Remove the file name
    (setq path (vl-filename-directory path))
    (while (setq pos (vl-string-search "\\" path))
      (setq path (substr path (+ 2 pos)))
      (setq lst (cons path lst))
    )
    lst
  ) ;; end PathListBelow

  ;; Compare two full paths.
  ;; Returns an interger which is the difference between the lengths
  ;; of the paths.
  (defun ComparePathLengths (docpath xrefpath / doccnt xrefcnt pos)
    (setq doccnt 0 xrefcnt 0)
    (while (setq pos (vl-string-search "\\" docpath))
      (setq doccnt (1+ doccnt))
      (setq docpath (substr docpath (+ 2 pos)))
    )
    (while (setq pos (vl-string-search "\\" xrefpath))
      (setq xrefcnt (1+ xrefcnt))
      (setq xrefpath (substr xrefpath (+ 2 pos)))
    )
    (fix (- (float doccnt) xrefcnt))
  ) ;; end ComparePathLengths

  ;; Used below in the RelativeFilePosition function.
  ;; Example path: "C:\\PROJECTS\\SAI Projects\\Plan 13 lv KMSPA.dwg"
  ;; Returns 3
  (defun PathCount (path / cnt pos)
    (setq cnt 0)
    (while (setq pos (vl-string-search "\\" path))
      (setq cnt (1+ cnt))
      (setq path (substr path (+ 2 pos)))
    )
    cnt
  ) ;; end PathCount

  ;; Arguments:
    ;; docpath is the full path to the active file
    ;; xrefpath is the full found at path to a reference    
  ;; Returns: "same" or "below" or "above" or "adjacent".
    ;; See inline comments.
  (defun RelativeFilePosition (docpath xrefpath / doccnt xrefcnt res)
    (setq docpath (vl-filename-directory docpath))
    (setq xrefpath (vl-filename-directory xrefpath))
    (setq doccnt (PathCount docpath))
    (setq xrefcnt (PathCount xrefpath))
    (cond
      ;; the two paths are equal after removing the file names 
      ;; and extensions
      ((eq (strcase docpath) (strcase xrefpath))
        (setq res "same")
      )
      ((and
         ;; the doc path is contained in the xref path
         ;; and the doc path is shorter than xref path.
         (vl-string-search (strcase docpath) (strcase xrefpath))
         (< doccnt xrefcnt)
        )
        (setq res "below")
      )
      ((and
         ;; the xref path is contained in the doc path
         ;; and the doc path is longer than xref path.
         (vl-string-search (strcase xrefpath) (strcase docpath))
         (> doccnt xrefcnt)
        )
        (setq res "above")
      )
      ;; else the result is adjacent
      (T
        (setq res "adjacent")
      )
    )
    res
  ) ;; end RelativeFilePosition

  ;;;; End functions common to both RelativeImages and RelativeXrefs ;;;;

  (defun RelativeImages (ss / cnt strlst str imglst imgfullpath imgpath 
                         curpath exten imgname imgfn templst tempcnt tempath 
                         origfn repath reportlst res filetype unloaded 
                         notfoundlst foundatdrivelst ImageIsUnloaded 
                         UnderlayIsUnloaded)

    ;; Returns an elist if an underlay is unloaded.
    (defun UnderlayIsUnloaded (curpath / ext dict item noload res)
      (setq ext (strcase (vl-filename-extension curpath)))
      (cond
        ((eq ext ".DWF")
          (setq dict (dictsearch (namedobjdict) "ACAD_DWFDEFINITIONS"))
        )
        ((eq ext ".DGN")
          (setq dict (dictsearch (namedobjdict) "ACAD_DGNDEFINITIONS"))
        )
        ((eq ext ".PDF")
          (setq dict (dictsearch (namedobjdict) "ACAD_PDFDEFINITIONS"))
        )
      )
      (foreach x dict
        (if 
          (and 
            (= 350 (car x))
            (setq item (entget (cdr x) '("ACAD")))
            (eq curpath (cdr (assoc 1 item)))
            (setq noload (assoc -3 item))
            (eq "NOLOAD" (cdr (assoc 1000 (cdadr noload))))
          )
          (setq res item)
        )
      )
      res
    ) ;; end UnderlayIsUnloaded

    ;; Returns an elist if an image is unloaded.
    ;; the relurned elist can be modified in the calling function.
    ;; to unload the image after it is reload due to repathing.
    ;; if the function returns nil the image is loaded and nothing needs 
    ;; to happen in the calling function.
    (defun ImageIsUnloaded (curpath / imgdct item res)
      (setq imgdct (dictsearch (namedobjdict) "ACAD_IMAGE_DICT"))
      (foreach x imgdct
        (if 
          (and 
            (= 350 (car x))
            (setq item (entget (cdr x)))
            (eq curpath (cdr (assoc 1 item)))
            (= 0 (cdr (assoc 280 item)))
          )
          (setq res item)
        )
      )
      res
    ) ;; end ImageIsUnloaded

    (setq cnt 0)
    (setq imglst (SSVLAList ss))
    (foreach x imglst 
      (setq imgpath nil templst nil tempath nil tempcnt nil 
            unloaded nil filetype nil)
      
      (if (vlax-property-available-p x 'ImageFile)
        (setq filetype "image")
        (setq filetype "underlayment") ;; PDF DWF DGN
      )

      (if (eq filetype "image")
        (setq curpath (vlax-get x 'ImageFile))
        (setq curpath (vlax-get x 'File)) ;; PDF DWF DGN
      )

      (setq exten (vl-filename-extension curpath))
      ;; This is the name given to the image reference w/o the file
      ;; file type extension. Not sure this is useful at this point.
      ;; not used at this time
      ;(setq imgname (vlax-get x 'Name))
      ;; This is the actual name of image file
      (setq imgfn (strcat (vl-filename-base curpath) exten))
      ;; because the variable imgfn may change below.
      (setq origfn imgfn)
      ;; this is the found at path if it exists.
      ;; NOTE, this is the full path including file name and extension.
      (setq imgfoundatpath (FilePath imgfn curpath))
      (if imgfoundatpath
        (progn
          (setq relpos (RelativeFilePosition docfullpath imgfoundatpath))
        )
      )
      (cond
        ;; File not found at the current path or within the active
        ;; file directory/folder. Do nothing.
        ((not imgfoundatpath)
          (setq notfoundlst (cons curpath notfoundlst))
        )

        ;; File found on a drive other than the
        ;; drive where the active file resides.
        ((and
           (not (IsRelativePath curpath))
           (not (eq (substr docfullpath 1 1) (substr curpath 1 1)))
           ;; Path is a full path.
           (vl-string-search "\\" curpath)
           (findfile curpath)
          )
          (setq foundatdrivelst (cons curpath foundatdrivelst))
        )
        ;; There is no path
        ;; The existing path is the same as the file name 
        ;; and the path is valid. No change, xpath is nil.
        ;; This works even when the xref was renamed.
        ;; Keep this as the first condition.
        ((and
          (eq (strcase imgfn) (strcase curpath))
          (findfile curpath)
         )
        )
        ;; The xref found at path is the same as the document path
        ;; and the xref saved path is valid.
        ((and
           (eq
            (strcase (vl-filename-directory docfullpath))
            (strcase (vl-filename-directory imgfoundatpath))
           )
           (findfile curpath)
          )
          (setq imgpath imgfn)
        )
        ;; The image found at path is the same as the document path
        ;; and the image saved path is not valid. Allow the user an option
        ;; to substitute a relative found at path for the saved path.
        ((and
           (eq
            (strcase (vl-filename-directory docfullpath))
            (strcase (vl-filename-directory imgfoundatpath))
           )
           (not (findfile curpath))
          )
          (textscr)
          (princ (strcat "\nInvalid saved path detected: " curpath))
          (princ (strcat "\nFound at: " imgfoundatpath))
          (initget "Yes No")
          (setq repath
            (getkword "\nAllow repath to the found at directory? [Yes/No] <Y>: ")
          )
          (if (or (eq repath "Yes") (not repath))
            (setq imgpath imgfn)
          )
        ) 
        ;; Condition specific for the situation where an unneed relative path
        ;; is removed. Example: the current path is ".\\<filename>".
        ((and
           (eq
            (strcase (vl-filename-directory docfullpath))
            (strcase (vl-filename-directory imgfoundatpath))
           )
           (IsRelativePath curpath)
           (findfile curpath)
          )
          (setq imgpath imgfn)
        )
        ;; Image found down the folder structure relative to the active doc.
        ((eq relpos "below")
          (setq lstpos (ComparePathLengths imgfoundatpath docfullpath))
          (setq strlst (PathListBelow imgfoundatpath))
          (setq imgfn (strcat ".\\" (nth (1- lstpos) strlst) "\\" imgfn))
          (setq imgpath imgfn)
        )
        ;; Image found above the folder structure relative to the active doc.
        ((eq relpos "above")
          (repeat (ComparePathLengths docfullpath imgfoundatpath)           
            (setq imgfn (strcat "..\\" imgfn))
          )
          (setq imgpath imgfn)
        )
        ;; Image is located in some adjacent folder.
        ((eq relpos "adjacent")          
          (setq templst (CompareAdjacentPaths docfullpath imgfoundatpath))
          (setq tempcnt (cadr templst))
          (setq tempath (car templst))
          (setq res "")
          (foreach x tempath
            (setq res (strcat res x))
          )
          ;; remove the leading \\  
          (setq res (substr res 2))
          (setq onedot "..\\")
          (setq dot "")
          (repeat tempcnt
            (setq dot (strcat dot onedot))
          )
          ;; New path
          (setq res (strcat dot res "\\" imgfn))
          (setq imgpath res)
        )
      )

      ;; Returns dictionary item elist if image file is unloaded, otherwise nil.
      (if (and imgpath (eq filetype "image"))
        (setq unloaded (ImageIsUnloaded curpath))
      )

      ;; Returns dictionary item elist if underlayment file is unloaded, otherwise nil.
      (if (and imgpath (eq filetype "underlayment"))
        (setq unloaded (UnderlayIsUnloaded curpath))
      )

      ;; If a relative path is found, check it is not the same 
      ;; as the original path.
      (if 
        (and 
          imgpath 
          (not (eq imgpath curpath))
          (findfile imgpath)
        )
        (progn
          (if (eq filetype "image")
            (vlax-put x 'ImageFile imgpath)    
            (vlax-put x 'File imgpath)
          )
        )
      )
      ;; Unloaded image
      (if (and unloaded (eq filetype "image"))
        ;; Remove the old path in elist and entmod the dictionary
        ;; entry. DXF code 280 is zero which means unloaded.
        (entmod (vl-remove (assoc 1 unloaded) unloaded))
      )
      ;; Unloaded underlay
      (if (and unloaded (eq filetype "underlayment"))
        (progn
          (setq unloaded (subst (cons 1 imgpath) (assoc 1 unloaded) unloaded))
          (entmod unloaded)
          (vlax-invoke doc 'Regen acAllViewports)
        )
      )
      (if (and imgpath (not (eq imgpath curpath)))
        (setq reportlst (cons (list origfn imgpath filetype) reportlst)
              cnt (1+ cnt)
        )
      )
    ) ;foreach
    ;; Report references not found
    (if notfoundlst
      (progn
        (princ "\nImages or underlays not found: ")
        (foreach x notfoundlst
          (princ (strcat " \n " x))
        )
        (print)
      )
    )   
    ;; Report references found on other drives
    (if foundatdrivelst
      (progn
        (princ "\nImages or underlays found on an alternate drive: ")
        (foreach x foundatdrivelst
          (princ (strcat " \n " x))
        )
        (print)
      )
    )
    ;; Main report:
    (foreach x reportlst
      (if (eq (last x) "image")
        (progn
          (princ (strcat "\nImage: " (car x) " repathed: \n"))
          (princ (strcat "  " (cadr x)))
        )
        (progn
          (princ (strcat "\nUnderlayment: " (car x) " repathed: \n"))
          (princ (strcat "  " (cadr x)))
        )
      )
    )
    ;; Report "images or underlayments" when the current version is 2007 or later.
    (if (< (atof (getvar "AcadVer")) 17.0)
      (princ (strcat "\n Number of images repathed: " (itoa cnt)))
      (princ (strcat "\n Number of images or underlayments repathed: " (itoa cnt)))
    )
  ) ;; end RelativeImages

  (defun RelativeXrefs ( / cnt datalst strlst fn xpath curpath xfullpath
                           xname reportlst res dot onedot nestcnt lstpos templst
                           tempcnt temppath relpos repath xrefdef unloadedflag 
                           notfoundlst foundatdrivelst XrefsData UnloadedXref)

    ;; Returns a list of lists: (fullpath fn blockname expath)
    ;; Example of one list in list of lists.
    ;("C:\\PROJECTS\\SAI Projects\\kauai lagoons spa condos 
    ;test\\Common ABC\\XRefs ABC\\Border 3624 KLSC.dwg" "Border 3624 KLSC.dwg" 
    ;"Border 3624 KLSC" "..\\XRefs ABC\\Border 3624 KLSC.dwg")
    (defun XrefsData ( / blkname expath fullpath fn xlst NestedXref)
      ;; Argument: block definition vla-object.
      ;; Returns a count number if the xref is nested, otherwise nil.
      ;; Based on code by Stephan Koster in a program named XrefTree.
      ;; Function renamed from nested_p.
      ;; The nestcnt variable is local to the primary routine.
      ;; There is a known flaw in the function which Jason pointed out.
      ;; If an xref is both nested and referenced as a parent, the
      ;; function does not flag it as nested. The fallout from that situation,
      ;; if it occurs, is handled near the end of the primary routine.
      (defun NestedXref (blkdef / elst) 
        (setq elst (entget (vlax-vla-object->ename blkdef)))
        (if
          (or
            (not (vl-position '(102 . "{BLKREFS") elst))
            (and
              (vl-position '(102 . "{BLKREFS") elst)
              (not (cdr (assoc 331 elst)))
            )
          )
          (setq nestcnt (1+ nestcnt))
          ;; Else return nil to the parent function.
        )
      ) ;end NestedXref

      ;; Added 5/3/2011.
      ;; Argument: xref block definition name - example: "Border 4230 KMR".
      ;; Returns: T if the xref is unloaded. 
      ;; This is used to determine whether an xref was unloaded before the
      ;; path is revised, which includes a call to the ActiveX Reload method.
      ;; If it returns T a flag is set and the ActiveX Unload method is 
      ;; subsequently called.
      (defun UnloadedXref (name / dxf70)
        (setq dxf70 (cdr (assoc 70 (tblsearch "block" name))))
        (= 0 (logand 32 dxf70))
      ) ;; end UnloadedXref

      (vlax-for x blocks
        (if 
          (and
            (= -1 (vlax-get x 'IsXref))
            (setq blkname (vlax-get x 'Name))
            ;; Filter out nested xrefs.
            (not (NestedXref x))
            (setq expath (vlax-get x 'Path))
            (setq fn (strcat (vl-filename-base expath) ".dwg"))
          )
          (progn
            ;; This returns nil if the file is not found at the current path
            ;; or within the active document folder. That's okay and handled elsewhere.
            (setq fullpath (FilePath fn expath))
            (setq xlst (cons (list fullpath fn blkname expath) xlst))
          )
        )
      )
      xlst
    ) ;; end XrefsData

    ;;(setq blocks (vla-get-blocks doc)
    (setq cnt 0
          nestcnt 0
          datalst (XrefsData)
    )

    ;; datalst is a list of lists
    ;; xref (fullname fn blkname expath)
    (foreach x datalst
      (setq
        ;; Full path to the xref.
        ;; This is the "found at" path. 
        ;; It might be nil since I'm using the FilePath function
        ;; in the XrefsData fuction now.
        xfullpath (car x)
        ;; Xref file name with .dwg.
        fn (cadr x)
        ;; The xref block name which is not the same as filename
        ;; when the xref was renamed.
        xname (caddr x)
        ;; New 5/2/2011 - the xref block definition.
        xrefdef (vla-item blocks xname)
        ;; This is the current "saved" path.
        curpath (cadddr x)
        xpath nil templst nil tempath nil tempcnt nil
      )
      (if xfullpath
        (setq relpos (RelativeFilePosition docfullpath xfullpath))
      )
      (cond
        ;; Reference not found.
        ((not xfullpath)
          (setq notfoundlst (cons curpath notfoundlst))
        )
        ;; Reference found on a drive other than the
        ;; drive where the active file resides.
        ((and
           (not (IsRelativePath curpath))
           (not (eq (substr docfullpath 1 1) (substr curpath 1 1)))
           (vl-string-search "\\" curpath)
           (findfile curpath)
          )
          (setq foundatdrivelst (cons curpath foundatdrivelst))
        )           
        ;; There is no path
        ;; The existing path is the same as the file name 
        ;; and the path is valid. No change, xpath is nil.
        ;; This works even when the xref was renamed.
        ;; Keep this as the first condition ;;;;;;;;;;;;;;
        ((and
          (eq (strcase fn) (strcase curpath))
          (findfile curpath)
         )
        )
        ;; Condition specific for the situation where an unneed relative path
        ;; is removed. Example: the current path is ".\\<filename>".
        ((and
           (eq
            (strcase (vl-filename-directory docfullpath))
            (strcase (vl-filename-directory xfullpath))
           )
           (IsRelativePath curpath)
           (findfile curpath)
          )
          (setq xpath fn)
        )
        ;; The xref found at path is the same as the document path
        ;; and the xref saved path is valid.
        ((and
           (eq
            (strcase (vl-filename-directory docfullpath))
            (strcase (vl-filename-directory xfullpath))
           )
           (findfile curpath)
          )
          (setq xpath fn)
        )
        ;; The xref found at path is the same as the document path
        ;; and the xref saved path is not valid. Allow the user an option
        ;; to substitute a relative found at path for the saved path.
        ((and
           (eq
            (strcase (vl-filename-directory docfullpath))
            (strcase (vl-filename-directory xfullpath))
           )
           (not (findfile curpath))
          )
          (textscr)
          (princ (strcat "\nInvalid saved path detected: " curpath))
          (princ (strcat " \nFound at: " xfullpath))
          (initget "Yes No")
          (setq repath
            (getkword "\nAllow repath to the found at directory? [Yes/No] <Y>: ")
          )
          (if (or (eq repath "Yes") (not repath))
            (setq xpath fn)
          )
        ) 
        ;; The existing path is relative.
        ;; Do not change an existing relative path if the path is valid
        ;; except for the condition 2 above.
        ((and
          (IsRelativePath curpath)
          (findfile curpath)
         )
        )
        ;; Xref found down the folder structure relative to the active doc.
        ((eq relpos "below")
          (setq lstpos (ComparePathLengths xfullpath docfullpath))
          (setq strlst (PathListBelow xfullpath))
          (setq fn (strcat ".\\" (nth (1- lstpos) strlst) "\\" fn))
          (setq xpath fn)
        ) 
        ;; Xref found above the folder structure relative to the active doc.
        ((eq relpos "above")
          (repeat (ComparePathLengths docfullpath xfullpath)
            (setq fn (strcat "..\\" fn))
          )
          (setq xpath fn)
        )
        ;; Xref is located in some adjacent folder.
        ;; This is the tricky one.
        ((eq relpos "adjacent")          
          (setq templst (CompareAdjacentPaths docfullpath xfullpath))
          (setq tempcnt (cadr templst))
          (setq tempath (car templst))
          (setq res "")
          (foreach x tempath
            (setq res (strcat res x))
          )
          ;; remove the leading \\  
          (setq res (substr res 2))
          (setq onedot "..\\")
          (setq dot "")
          (repeat tempcnt
            (setq dot (strcat dot onedot))
          )
          ;; New path
          (setq res (strcat dot res "\\" fn))
          (setq xpath res)
        )
      ) ;cond

      (if 
        (and
          xpath
          (findfile xpath)
          ;; Check the path found is not the same as the original path.
          (not (eq xpath curpath))
        )
        (progn
          ;(command "._xref" "_path" xname xpath)
          ;(vlax-put xname 'Path xpath)
          ;; This seems to work consistent with RelativeImages and seems
          ;; to make undo work as expected. That did not work as expected
          ;; when the command call was used for some unkown reason.
          (setq unloadedflag (UnloadedXref xname))
          (vlax-put xrefdef 'Path xpath)
          (vlax-invoke xrefdef 'Reload)
          (if unloadedflag (vlax-invoke xrefdef 'Unload))
          (setq reportlst (cons (list xname xpath) reportlst)
                cnt (1+ cnt)
          )
        )
      )
    ) ; foreach
    ;; Report added 5/11/2011
    (if notfoundlst
      (progn
        (princ "\nXrefs not found: ")
        (foreach x notfoundlst
          (princ (strcat " \n " x))
        )
        (print)
      )
    )   
    ;; Report added 5/11/2011
    (if foundatdrivelst
      (progn
        (princ "\nXrefs found on an alternate drive: ")
        (foreach x foundatdrivelst
          (princ (strcat " \n " x))
        )
        (print)
      )
    )
    ;; Report
    (foreach x reportlst
      (princ (strcat "\nXref: " (car x) " repathed: " "\n"))
      (princ (strcat "  " (cadr x)))
    )
    (if (not (zerop nestcnt))
      (princ (strcat "\n Number of nested xrefs found: " (itoa nestcnt)))
    )
    (princ (strcat "\n Number of xrefs repathed: " (itoa cnt)))

  ) ;end RelativeXrefs

  ;;;; END SUB-FUNCTIONS ;;;;

  ;;;; MAIN ROUTINE ;;;;

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
        blocks (vla-get-blocks doc)
        docfullpath (vlax-get doc 'FullName)
        version (atof (getvar "AcadVer"))
        nestedthings (NestedStuff)
  )

  ;; Moved up 5/30/2011. Do this report here first and remove it from the
  ;; version specific condition statement below.
  (foreach x nestedthings
    (princ (strcat "\n Number of nested " (cdr x) " files found: " (itoa (car x))))
  )

  (setq ss (ssget "x" '((0 . "IMAGE,PDFUNDERLAY,DGNUNDERLAY,DWFUNDERLAY"))))

  (vla-StartUndoMark doc)

  (cond
    ;; Just run RelativeXrefs without presenting file type options when
    ;; other file type references are not found.
    ((not ss)
      (RelativeXrefs)
    )
    ;; ACAD 2009 or later. Note, PDF underlays were added in 2010, but there was
    ;; a subscription bonus pack which allowed them to work in 2009.
    ((>= version 17.2)
      (initget "Xref Dgn dwF Pdf Images All")
      (setq what (getkword "\nSelect file type or All: [Xref/Dgn/dwF/Images/All] <All>: "))
      (cond
        ((or (not what) (eq what "All"))
          (RelativeImages ss)
          (RelativeXrefs)
        ) ; All
        ((eq what "Xref") (RelativeXrefs))
        ((eq what "Dgn")
          (if (setq ss (ssget "x" '((0 . "DGNUNDERLAY")))) 
            (RelativeImages ss)
            (princ "\nNo DGN references found. ")
          )
        )
        ((eq what "dwF")
          (if (setq ss (ssget "x" '((0 . "DWFUNDERLAY")))) 
            (RelativeImages ss)
            (princ "\nNo DWF references found. ")
          )
        )
        ((eq what "Pdf")
          (if (setq ss (ssget "x" '((0 . "PDFUNDERLAY")))) 
            (RelativeImages ss)
            (princ "\nNo PDF references found. ")
          )
        )
        ((eq what "Images")
          (if (setq ss (ssget "x" '((0 . "IMAGE")))) 
            (RelativeImages ss)
            (princ "\nNo Image references found. ")
          )
        )
      )
    ) ; 2009 or later
    ;; ACAD 2008 - DGN added
    ((= version 17.1)
      (initget "Xref Dgn dwF Images All")
      (setq what (getkword "\nSelect file type or All: [Xref/Dgn/dwF/Images/All] <All>: "))
      (cond
        ((or (not what) (eq what "All"))
          (RelativeImages ss)
          (RelativeXrefs)
        ) ; All
        ((eq what "Xref") (RelativeXrefs))
        ((eq what "Dgn")
          (if (setq ss (ssget "x" '((0 . "DGNUNDERLAY")))) 
            (RelativeImages ss)
            (princ "\nNo DGN references found. ")
          )
        )
        ((eq what "dwF")
          (if (setq ss (ssget "x" '((0 . "DWFUNDERLAY")))) 
            (RelativeImages ss)
            (princ "\nNo DWF references found. ")
          )
        )
        ((eq what "Images")
          (if (setq ss (ssget "x" '((0 . "IMAGE")))) 
            (RelativeImages ss)
            (princ "\nNo Image references found. ")
          )
        )
      )
    ) ; 2008
    ;; ACAD 2007 - DWF added
    ((= version 17.0)
      (initget "Xref dwF Images All")
      (setq what (getkword "\nSelect file type or All: [Xref/dwF/Images/All] <All>: "))
      (cond
        ((or (not what) (eq what "All"))
          (RelativeImages ss)
          (RelativeXrefs)
        ) ; All
        ((eq what "Xref") (RelativeXrefs))
        ((eq what "dwF")
          (if (setq ss (ssget "x" '((0 . "DWFUNDERLAY")))) 
            (RelativeImages ss)
            (princ "\nNo DWF references found. ")
          )
        )
        ((eq what "Images")
          (if (setq ss (ssget "x" '((0 . "IMAGE")))) 
            (RelativeImages ss)
            (princ "\nNo Image references found. ")
          )
        )
      )
    ) ; 2007
    ;; ACAD 2000 to 2006 - only images and DWG references
    ((<= 15.0 version 16.2)
      (initget "Xref Images All")
      (setq what (getkword "\nSelect file type or All: [Xref/Images/All] <All>: "))
      (cond
        ((or (not what) (eq what "All"))
          (RelativeImages ss)
          (RelativeXrefs)
        ) ; All
        ((eq what "Xref") (RelativeXrefs))
        ((eq what "Images")
          (if (setq ss (ssget "x" '((0 . "IMAGE")))) 
            (RelativeImages ss)
            (princ "\nNo Image references found. ")
          )
        )
      )
    ) ; 2000 to 2006
    ;; Versions prior to ACAD 2000 - exit.
    (T
      (princ "\nACAD 2000 or later required. Exiting... ")
      (exit)
    )
  ) ; cond

  (*error* nil)
) ;end RelativePaths

;------------------------------------
;shortcut
(defun c:REP ( / objs num n n1 obj ent bln chk chk2)
	(setq objs (ssget "X" '((0 . "INSERT"))))
	(setq num (sslength objs))
	(setq n 0 n1 0)
	
	(repeat num
		(setq obj (ssname objs n))
		(setq ent (entget obj))
		(setq bln (cdr (assoc 2 ent)))
		(setq chk (assoc 1 (tblsearch "block" bln)))
		(setq chk2 (substr (cdr chk) 1 1))
		(if (or (/= chk "") (/= chk2 "."))
			(setq n1 (+ n1 1))
		)
		(setq n (+ n 1))
	)
	(repeat n1 
		(c:RelativePaths)
	)
)
;------------------------------------