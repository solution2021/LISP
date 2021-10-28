(defun c:kk (/ cl cn cc new)

  (setq cl (getvar "CLAYER"))

     (setq cn (tblobjname "layer" cl))
  (setq cc (abs (cdr (assoc 62 (entget cn)))))

  (if (> (+ cc 1) 7) (setq cc 0))
  
  (setq lname (getstring "\nEnter the name of new current layer:"))
  (command "-layer" "m" lname "c" (+ cc 1) "" "")


)
