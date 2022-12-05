;;; CADALYST 09/06  Tip 2149: QuickProfile.lsp  Center Line Profile	(c) 2006 Pedro Ferreira 


;;;Author: Pedro Miguel da Silva Ferreira	Email:pedro_ferreira@netcabo.pt or pferreira@wsatkins.pt
;;;Web page: http:pwp.netcabo.pt/pedro_ferreira
;;;Location: Portugal, Lisboa
;;;RDS: PMSF
;;;Command Name: qp
;;;Date: 09 of May 2006
;;;Version: 1.0
;;;Description: Visual Lisp Routine that creates a section profile of the terrain based on the existing contours.



(defun timeini ()
  (setq s (getvar "DATE"))
  (setq seconds (* 86400.0 (- s (fix s))))
)

(defun timeend ()
  (setq s1 (getvar "DATE"))
  (setq seconds1 (* 86400.0 (- s1 (fix s1))))
  (setq seconds2 (fix (- seconds1 seconds)))
  (princ
    (strcat "\nTime : "
	    (itoa seconds2)
	    " seconds"
    )
  )
)

(defun inivar ()
  (setq	cmd_ini	(getvar "cmdecho")
	;fla_ini	(getvar "flatland")
	osm_ini	(getvar "osmode")
	ort_ini	(getvar "orthomode")
	plt_ini	(getvar "plinetype")
	aup_ini	(getvar "auprec")
	uni_ini	(getvar "unitmode")
	lun_ini	(getvar "lunits")
	diz_ini	(getvar "dimzin")
	edg_ini	(getvar "edgemode")
  )
  (setvar "CMDECHO" 0)
  ;(setvar "FLATLAND" 0)
  (setvar "OSMODE" 0)
  (setvar "ORTHOMODE" 0)
  (setvar "PLINETYPE" 2)
  (setvar "AUPREC" 0)
  (setvar "UNITMODE" 1)
  (setvar "LUNITS" 2)
  (setvar "DIMZIN" 0)
  (setvar "EDGEMODE" 1)
)

(defun recvar ()
  (setvar "CMDECHO" cmd_ini)
  ;(setvar "FLATLAND" fla_ini)
  (setvar "OSMODE" osm_ini)
  (setvar "ORTHOMODE" ort_ini)
  (setvar "PLINETYPE" plt_ini)
  (setvar "AUPREC" aup_ini)
  (setvar "UNITMODE" uni_ini)
  (setvar "LUNITS" lun_ini)
  (setvar "DIMZIN" diz_ini)
  (setvar "EDGEMODE" edg_ini)
)

(defun getlayname ()
  (setq contourstest nil)
  (setq	layername
	 (getstring
	   "\nPlease enter the layer name of the contours: "
	 )
  )
  (setq	contourstest
	 (ssget	"_x"
		(list (cons -4 "<OR")
		      (cons -4 "<AND")
		      (cons 0 "lwpolyline")
		      (cons 8 layername)
		      (cons -4 "AND>")
		      (cons -4 "<AND")
		      (cons 0 "polyline")
		      (cons 8 layername)
		      (cons -4 "AND>")
		      (cons -4 "<AND")
		      (cons 0 "line")
		      (cons 8 layername)
		      (cons -4 "AND>")
		      (cons -4 "<AND")
		      (cons 0 "spline")
		      (cons 8 layername)
		      (cons -4 "AND>")
		      (cons -4 "OR>")
		)
	 )
  )

  (while (= contourstest nil)
    (princ "\nNo contours selected...")
    (setq layername
	   (getstring
	     "\nPlease enter the layer name of the contours: "
	   )
    )
    (setq contourstest
	   (ssget "_x"
		  (list	(cons -4 "<OR")
			(cons -4 "<AND")
			(cons 0 "lwpolyline")
			(cons 8 layername)
			(cons -4 "AND>")
			(cons -4 "<AND")
			(cons 0 "polyline")
			(cons 8 layername)
			(cons -4 "AND>")
			(cons -4 "<AND")
			(cons 0 "line")
			(cons 8 layername)
			(cons -4 "AND>")
			(cons -4 "<AND")
			(cons 0 "spline")
			(cons 8 layername)
			(cons -4 "AND>")
			(cons -4 "OR>")
		  )
	   )
    )
  )
)

(defun activexsupport ()
  (vl-load-com)
  (setq	*modelspace*
	 (vla-get-modelspace
	   (vla-get-activedocument (vlax-get-acad-object))
	 )
  )
)

(defun esttexto	()

  (vl-cmdf "._style" "PMSF-TEXT" "romans" 2.50 0.80 0 "n" "n" "n")
)

(defun getha ()
  ;; this entity must be a lwpolyline
  (activexsupport)
  (setq
    ha (entsel "\nSelect the Horizontal alignment: ")
  )
  (while (= ha nil)
    (progn
      (princ "\nNothing selected...")
      (setq ha
	     (entsel "\nSelect the Horizontal alignment: ")
      )
    )
  )
  (setq ha-type (cdr (assoc 0 (entget (car ha)))))
  (if (not (equal ha-type "LWPOLYLINE"))
    (progn
      (setq ha nil)
      (princ "\n***Horizontal Alignment must be a LWPolyline***")
    )
  )
  (while (= ha nil)
    (progn
      (princ "\nNothing selected...")
      (setq ha
	     (entsel "\nSelect the Horizontal alignment: ")
      )
      (setq ha-type (cdr (assoc 0 (entget (car ha)))))
      (if (not (equal ha-type "LWPOLYLINE"))
	(progn
	  (setq ha nil)
	  (princ "\n***Horizontal Alignment must be a LWPolyline***")
	)
      )
    )
  )
  (setq ha-ename (entget (car ha)))
  (setq ha-ename (cdr (assoc -1 ha-ename)))
  (setq ha-object (vlax-ename->vla-object ha-ename))

  (vl-cmdf "._text"
	   (vlax-curve-getstartpoint ha-object)
	   "0"
	   "A"
  )
  (vl-cmdf "._text"
	   (vlax-curve-getendpoint ha-object)
	   "0"
	   "B"
  )
)

(defun getexaggeration ()
  (initget 2)
  (setq ve (getreal "\nEnter the vertical exaggeration <1>: "))
  (if (= ve nil)
    (setq ve 1)
  )
)


(defun listptintersect ()
  (setq listaxy nil)

  (setq hazvalue (caddr (vlax-curve-getStartPoint ha-object)))

  (setq curvas contourstest)
  (setq ncurvas (sslength curvas))
  (setq listaxy nil)
  (setq counter 0)
  (while (< counter ncurvas)
    (progn
      (setq cnivel-ename (ssname curvas counter))
      (setq cnivel-object (vlax-ename->vla-object cnivel-ename))

      (setq cnivelzvalue
	     (caddr (vlax-curve-getStartPoint cnivel-object))
      )

      (setq ha-ENTITY
	     (subst (cons 38 cnivelzvalue)
		    (assoc 38 (entget (car ha)))
		    (entget (car ha))
	     )
      )
      (entmod ha-ENTITY)

      (setq intersectpt
	     (vlax-variant-value
	       (vlax-invoke-method
		 ha-object
		 "IntersectWith"
		 cnivel-object
		 acExtendNone
	       )
	     )
      )

      (setq test nil)
      (setq
	test (vl-catch-all-apply
	       'vlax-safearray->list
	       (list intersectpt)
	     )
      )
      (setq error (vl-catch-all-error-p test))

      (if (/= error t)
	(progn
	  (setq intersectpt (vlax-safearray->list intersectpt))
	  (setq interlength (length intersectpt))

	  (if (> interlength 3)
	    (progn
	      (setq dividelength (/ interlength 3))
	      (setq count 0)
	      (while (< count interlength)
		(progn
		  (setq	newpt (list (nth count intersectpt)
				    (nth (+ count 1) intersectpt)
				    (nth (+ count 2) intersectpt)
			      )
		  )

		  (setq x (vlax-curve-getdistatPoint ha-ename newpt))
		  (setq z (caddr intersectpt))
		  (setq xy (list x (* z ve)))
		  (setq
		    listaxy (append listaxy (list xy))
		  )

		  (setq count (+ count 3))
		)
	      )
	    )
	    (progn
	      (setq x (vlax-curve-getdistatPoint ha-ename intersectpt))
	      (setq z (caddr intersectpt))
	      (setq xy (list x (* z ve)))
	      (setq
		listaxy	(append listaxy (list xy))
	      )
	    )
	  )

	  (setq	ha-ENTITY
		 (subst	(cons 38 hazvalue)
			(assoc 38 (entget (car ha)))
			(entget (car ha))
		 )
	  )
	  (entmod ha-ENTITY)
	)
      )
      (setq counter (1+ counter))
    )
  )

  (setq	listaxy
	 (vl-sort listaxy
		  (function (lambda (e1 e2)
			      (< (car e1) (car e2))
			    )
		  )
	 )
  )

  (setq	startdist (vlax-curve-getdistatPoint
		    ha-ename
		    (vlax-curve-getstartpoint ha-ename)
		  )
	enddist	  (vlax-curve-getdistatPoint
		    ha-ename
		    (vlax-curve-getendpoint ha-ename)
		  )
  )

  (setq	pt1 (car (car listaxy))
	pt2 (car (last listaxy))
  )

  (if (/= startdist pt1)
    (progn
      (setq x startdist)
      (setq y (+ (* (/ (- (cadr (car listaxy)) (cadr (cadr listaxy)))
		       (- (car (cadr listaxy)) (car (car listaxy)))
		    )
		    (- (car (car listaxy)) startdist)
		 )
		 (cadr (car listaxy))
	      )
      )
      (setq xy (list x y))
      (setq
	listaxy	(append listaxy (list xy))
      )
      (setq listaxy
	     (vl-sort listaxy
		      (function	(lambda	(e1 e2)
				  (< (car e1) (car e2))
				)
		      )
	     )
      )

    )
  )

  (if (/= enddist pt1)
    (progn
      (setq pos (1- (length listaxy)))
      (setq x enddist)
      (setq y
	     (+
	       (*
		 (/ (- (cadr (nth pos listaxy))
		       (cadr (nth (1- pos) listaxy))
		    )
		    (- (car (nth pos listaxy)) (car (nth (1- pos) listaxy)))
		 )
		 (- enddist (car (nth pos listaxy)))
	       )
	       (cadr (nth pos listaxy))
	     )
      )
      (setq xy (list x y))
      (setq
	listaxy	(append listaxy (list xy))
      )
      (setq listaxy
	     (vl-sort listaxy
		      (function	(lambda	(e1 e2)
				  (< (car e1) (car e2))
				)
		      )
	     )
      )

    )
  )
)

(defun createprofile ()
  (setq variante-listaxy (apply 'append listaxy))

  (setq	arraySpace
	 (vlax-make-safearray
	   vlax-vbdouble
	   (cons 0
		 (- (length variante-listaxy) 1)
	   )
	 )
  )
  (setq	variante-listaxy
	 (vlax-safearray-fill arraySpace variante-listaxy)
  )

  (vlax-make-variant variante-listaxy)


  (setq	pline (vla-addLightweightPolyline
		*ModelSpace*
		variante-listaxy
	      )
  )


  (vl-cmdf "._text"
	   (vlax-curve-getstartpoint pline)
	   "0"
	   "A"
  )
  (vl-cmdf "._text"
	   (vlax-curve-getendpoint pline)
	   "0"
	   "B"
  )
)

(defun annotate	()
  (setq	xini (car (vlax-curve-getstartpoint pline))
	xend (car (vlax-curve-getendpoint pline))
	y    (*	(fix
		  (/ (cadr (car	(vl-sort listaxy
					 (function (lambda (e1 e2)
						     (< (cadr e1) (cadr e2))
						   )
					 )
				)
			   )
		     )
		     ve
		  )
		)
		ve
	     )
  )
  ;;end setq

  (if (< y 0)
    (setq y (- y (* 1 ve)))
  )

  (setq var-xyini (apply 'append (list (list xini y 0))))
  (setq var-xyend (apply 'append (list (list xend y 0))))
  (createline)
  (setq yref (strcat "REFERENCE: " (rtos (/ y ve) 2 2)))
  (setq ptloc (list (- xini 30.0) y))
  (vl-cmdf "._text" ptloc "0" yref)




  (setq lengthlistaxy (length listaxy))
  (setq count 0)
  (while (< count lengthlistaxy)
    (progn
      (setq var-xyini (apply 'append
			     (list (list (car (nth count listaxy))
					 (cadr (nth count listaxy))
					 0
				   )
			     )
		      )
      )
      (setq
	var-xyend (apply 'append
			 (list (list (car (nth count listaxy)) y 0))
		  )
      )
      (createline)

      (setq ytext (rtos (/ (cadr (nth count listaxy)) ve) 2 2))
      (setq xpt (car (nth count listaxy)))
      (setq ptloc (list xpt (- y 10.0)))
      (vl-cmdf "._text" ptloc "90" ytext)

      (setq count (1+ count))
    )
  )




)



(defun createline ()

  (setq	arraySpace
	 (vlax-make-safearray
	   vlax-vbdouble
	   (cons 0
		 (- (length var-xyini) 1)
	   )
	 )
  )
  (setq	var-xyini
	 (vlax-safearray-fill arraySpace var-xyini)
  )

  (vlax-make-variant var-xyini)

  (setq	arraySpace
	 (vlax-make-safearray
	   vlax-vbdouble
	   (cons 0
		 (- (length var-xyend) 1)
	   )
	 )
  )
  (setq	var-xyend
	 (vlax-safearray-fill arraySpace var-xyend)
  )

  (vlax-make-variant var-xyend)

  (setq	line (vla-addline
	       *ModelSpace*
	       var-xyini
	       var-xyend
	     )
  )

)




(defun c:qp ()
  (timeini)
  (inivar)
  (getlayname)
  (esttexto)
  (getha)
  (getexaggeration)
  (listptintersect)
  (createprofile)
  (annotate)

  (vl-cmdf "._zoom"
	   (vlax-curve-getstartpoint pline)
	   (vlax-curve-getendpoint pline)
  )
  (recvar)
  (timeend)
  (princ)
)

(alert
  "Type [qp] in the command line\n\nAuthor: Pedro Ferreira\nweb page: http://pwp.netcabo.pt/pedro_Ferreira\n\nTHIS PROGRAM IS PROVIDED \"AS IS\" AND WITH ALL FAULTS.\n\nPress OK to continue."
)





