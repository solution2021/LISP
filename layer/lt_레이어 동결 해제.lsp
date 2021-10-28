; Layer all Thaw *********************************** 
(defun C:lt () 
(setvar "cmdecho" 0)
(prompt "\n.... setting Layer Thaw")
(command "layer" "on" "*" "") 
(command "layer" "thaw" "*" "") 
(princ)
)  

