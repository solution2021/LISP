; -------------------- ISOLATE LAYER FUNCTION --------------------
; Isolates selected object's layer by turning all other layers off
; ----------------------------------------------------------------

(Defun C:Lr (/ SS CNT LAY LAYLST VAL)

       (prompt "\nSelect object(s) on the layer(s) to be ISOLATED: ")
      (setq SS (ssget))

  (if SS
    (progn

      (setq CNT 0)

      (while (setq LAY (ssname SS CNT))
        (setq LAY (cdr (assoc 8 (entget LAY))))
        (if (not (member LAY LAYLST))
          (setq LAYLST (cons LAY LAYLST))
        )
        (setq CNT (1+ CNT))
      )

      (if (member (getvar "CLAYER") LAYLST)
        (setq LAY (getvar "CLAYER"))
        (setvar "CLAYER" (setq LAY (last LAYLST)))
      )

      (command "_.-LAYER" "_OFF" "*" "_Y")
      (foreach VAL LAYLST (command "_ON" VAL))
      (command "")
      
      (if (= (length LAYLST) 1)
        (prompt (strcat "\nLayer " (car LAYLST) " has been isolated."))
        (prompt (strcat "\n" (itoa (length LAYLST)) " layers have been isolated. "
                        "Layer " LAY " is current."
                )
        )
      )
    )
  )

  (princ)
)