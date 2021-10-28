;;----------------------=={ Block Count }==-------------------;;
;;                                                            ;;
;;  Generates a report detailing the number of all (or a      ;;
;;  selection of) primary and nested blocks, dynamic blocks   ;;
;;  and xrefs in a drawing.                                   ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright ?2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Version 1.2    -    12-12-2011                            ;;
;;------------------------------------------------------------;;

(defun c:BLC

    (
        /
        _GetBlockCount
        _PrintReport
        _Main
    )

    (defun _GetBlockCount

        (
            selection
            /
            _Assoc++
            _BlockHierarchy
            _GetBlockHierarchy
            _EffectiveName
            _UpdateNestedBlockCount
            _IterateSelection
            _Main
        )

        (defun _Assoc++ ( key value lst / pair )
            (if (setq pair (assoc key lst))
                (subst (cons key (+ value (cdr pair))) pair lst)
                (cons  (cons key value) lst)
            )
        )

        (defun _BlockHierarchy ( block / alist )
            (while block
                (if (eq "INSERT" (cdr (assoc 0 (entget block))))
                    (setq alist (_Assoc++ (_EffectiveName block) 1 alist))
                )
                (setq block (entnext block))
            )
            alist
        )

        (defun _GetBlockHierarchy ( / block name tree )
            (while (setq block (tblnext "BLOCK" (null block)))
                (setq tree
                    (cons
                        (cons
                            (setq name (cdr (assoc 2 block)))
                            (_BlockHierarchy (tblobjname "BLOCK" name))
                        )
                        tree
                    )
                )
            )
            tree
        )

        (defun _EffectiveName ( blockentity / name repbtag )
            (if (wcmatch (setq name (cdr (assoc 2 (entget blockentity)))) "`**")
                (if
                    (and
                        (setq repbtag
                            (cdadr
                                (assoc -3
                                    (entget
                                        (cdr
                                            (assoc 330
                                                (entget (tblobjname "BLOCK" name))
                                            )
                                        )
                                       '("AcDbBlockRepBTag")
                                    )
                                )
                            )
                        )
                        (setq repbtag (handent (cdr (assoc 1005 repbtag))))
                    )
                    (setq name (cdr (assoc 2 (entget repbtag))))
                )
            )
            name
        )

        (defun _UpdateNestedBlockCount ( name count tree alist / nests )
            (if (setq nests (cdr (assoc name tree)))
                (foreach nest nests
                    (setq alist
                        (_UpdateNestedBlockCount (car nest) (cdr nest) tree
                            (_Assoc++
                                (car nest)
                                (* count (cdr nest))
                                alist
                            )
                        )
                    )
                )
                alist
            )
        )

        (defun _IterateSelection ( selection blocktree / inc name nested primary )
            (if selection
                (repeat (setq inc (sslength selection))
                    (setq name    (_EffectiveName (ssname selection (setq inc (1- inc))))
                          primary (_Assoc++ name 1 primary)
                          nested  (_UpdateNestedBlockCount name 1 blocktree nested)
                    )
                )
            )
            (list primary nested)
        )

        (_IterateSelection selection (_GetBlockHierarchy))
    )

    (defun _PrintReport

        (
            data
            /
            _PadBetween
            _PrintIt
        )

        (defun _PadBetween ( s1 s2 ch ln )
            (
                (lambda ( a b c )
                    (repeat (- ln (length b) (length c)) (setq c (cons a c)))
                    (vl-list->string (append b c))
                )
                (ascii ch)
                (vl-string->list s1)
                (vl-string->list s2)
            )
        )

        (defun _PrintIt ( lst wid )
            (princ (_PadBetween "\n" "" "=" wid))
            (princ "\n Block Count")
            (princ (_PadBetween "\n" "" "=" wid))
            (princ
                (_PadBetween
                    (strcat
                        "\n Primary Blocks ("
                        (itoa (apply '+ (mapcar 'cdr (car lst))))
                        ")"
                    )
                    "Count" " " wid
                )
            )
            (princ (_PadBetween "\n" "" "-" wid))
            (foreach item
                (vl-sort
                    (car lst)
                    (function (lambda ( a b ) (< (car a) (car b))))
                )
                (princ (_PadBetween (strcat "\n " (car item)) (itoa (cdr item)) "." wid))
            )
            (if (cadr lst)
                (progn
                    (princ (_PadBetween "\n" "" "=" wid))
                    (princ
                        (_PadBetween
                            (strcat
                                "\n Nested Blocks ("
                                (itoa (apply '+ (mapcar 'cdr (cadr lst))))
                                ")"
                            )
                            "Count" " " wid
                        )
                    )
                    (princ (_PadBetween "\n" "" "-" wid))
                    (foreach item
                        (vl-sort
                            (cadr lst)
                            (function (lambda ( a b ) (< (car a) (car b))))
                        )
                        (princ (_PadBetween (strcat "\n " (car item)) (itoa (cdr item)) "." wid))
                    )
                )
            )
            (princ (_PadBetween "\n" "" "=" wid))
        )

        (_PrintIt data 50)
    )

    (defun _Main ( / allblocks sel )
        (cond
            (   (null (setq allblocks (ssget "_X" '((0 . "INSERT")))))
                (princ "\nNo Blocks Found in Drawing.")
            )
            (   (progn
                    (setvar 'NOMUTT 1)
                    (princ "\nSelect Blocks to Count <All>: ")
                    (setq sel
                        (cond
                            (   (null (setq sel (vl-catch-all-apply 'ssget '(((0 . "INSERT"))))))
                                allblocks
                            )
                            (   (null (vl-catch-all-error-p sel))
                                sel
                            )
                        )
                    )
                    (setvar 'NOMUTT 0)
                    sel
                )
                (_PrintReport (_GetBlockCount sel))
                (textpage)
            )
        )
        (princ)
    )
    
    (_Main)
)
(vl-load-com)
(princ "\n:: BlockCount.lsp | Version 1.2 | ?Lee Mac 2011 www.lee-mac.com ::")
(princ "\n:: Type \"BlockCount\" to Invoke ::")
(princ)

;;------------------------------------------------------------;;
;;                         End of File                        ;;
;;------------------------------------------------------------;;