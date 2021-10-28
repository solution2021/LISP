;;;블럭 모두 폭파
(defun c:bxa( / loop ss)
    (vl-load-com)
    (defun loop (obs)
        (if obs
            (progn
                (if (= (vlax-get (car obs) 'objectname) "AcDbBlockReference")
                    (progn
                        (loop (vlax-safearray->list (vlax-variant-value (vla-explode (car obs)))))
                        (vla-delete (car obs))
                    )
                )
                (loop (cdr obs))
            )
        )
    )

    (if (setq ss (ssget "_:L" '((0 . "insert"))))
        (loop
            (mapcar
                'vlax-ename->vla-object
                (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))
            )
        )
    )
    (princ)
)

;;;선택 블럭의 내부 모두 폭파
(defun c:bx( / loop ss)
    (vl-load-com)
    (defun loop (ob)
        (if (= (vlax-get ob 'objectname) "AcDbBlockReference")
            (progn
                (foreach o (vlax-safearray->list (vlax-variant-value (vla-explode ob)))
                    (loop o)
                )
                (vla-delete ob)
            )
        )
    )
    (if (setq ss (ssget '((0 . "insert"))))
        (progn
            (foreach en (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))
                (if (not (member (setq bn (cdr (assoc 2 (entget en)))) bns))
                    (setq bns (cons bn bns))
                )
            )

            (setq Blocks (vla-get-Blocks (vla-get-activedocument (vlax-get-acad-object))))
            (foreach bn bns
                (vlax-for block (vla-item blocks bn)
                    (loop block)
                )
            )
        )
    )
    (princ)
)