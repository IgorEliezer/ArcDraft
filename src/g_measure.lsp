;;;; g_measure.lsp
;;;; General commands for measurements, reading and math.
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2018-03-27
;;;; License: ArcDraft, see LICENSE.txt.


;;;; Set up commands for measurements, text parsing and math.


;;; ---- COMMANDS ----

;;; COMMAND: Insert coordenates

(defun c:ico (/ coord pref_x pref_y pt1 pt2 pta ptins x y)
  (prompt "\nICO - Inserir coordenadas X e Y")
  (ad:inicmd)

  ;; User input
  (if
    (setq pt1 (getpoint "\nClique num ponto para obter as coordenadas: "))
     (progn
       (setq pt2 (getpoint pt1 "\nClique no segundo ponto para linha de chamada: "))

       ;; Build string
       (setq x	    (rtos (car pt1))
	     y	    (rtos (cadr pt1))
	     pref_x "X = "
	     pref_y "Y = "
       )
       (setq coord (strcat pref_x x " ; " pref_y y))

       ;; 3rd point for leader line
       (if (<= (car pt1) (car pt2))	; if rightward
	 (setq pta (append (list (+ (car pt2) (* (strlen coord) 0.66 *ad:sc*))) (cdr pt2)))
	 (setq pta (append (list (- (car pt2) (* (strlen coord) 0.66 *ad:sc*))) (cdr pt2)))
       )

       ;; Draw leader line
       (command "_pline" pt1 pt2 pta "")

       ;; Insert text
       (setvar "OSMODE" 0)
       (setq ptins (ad:ptmed pt2 pta))
       (ad:text nil "_bc" ptins (* *ad:th* *ad:sc*) nil coord)
     )
  )

  (ad:endcmd)
  (princ)
)


;;; COMMAND: Sum LWPOLYLINEs

(defun c:sompl (/ ent i len ptins ss str_total total)
  (prompt "\nSOMPL - Somar comprimentos de polilinhas abertas")
  (ad:inicmd)

  ;; User input
  (setq	ss    (ad:ssgetp
		'((0 . "LWPOLYLINE") (-4 . "<or") (70 . 0) (70 . 128) (-4 . "or>")) ; open LWPL
		"\nSelecione polilinhas para medir (somente as abertas ser�o consideradas), e <ENTER> para concluir: "
	      )
	i     0
	total 0.00
  )

  ;; Sum
  (if ss
    (progn
      (while
	(setq ent (ssname ss i))
	 (setq len   (ad:pllen ent)
	       total (+ total len)
	       i     (1+ i)
	 )
      )

      ;; Prompt the user
      (setq str_total (rtos total))
      (prompt (strcat "\nValor total: " str_total "."))

      ;; Insert the text
      (setvar "OSMODE" 0)
      (if
	(setq ptins (getpoint " Clique para inserir o texto com o valor total ou <sair>: "))
	 (ad:text nil "_mc" ptins (* *ad:th* *ad:sc*) 0 str_total)
      )
    )
    (prompt "\nNenhuma polilinha aberta foi selecionada.")
  )

  (ad:endcmd)
  (princ)
)


;;; COMMAND: Sum numeric values from texts
;;; 	TO-DO: Move it to a proper module.

(defun c:somt (/ ent height i ptins ss str_total total value)
  (prompt "\nSOMT - Somar valores num�ricos de textos")
  (ad:inicmd)

  ;; User input
  (setq	ss    (ad:ssgetp
		'((0 . "TEXT") (1 . "#*,[+]#*,[`-]#*")) ; also accepts + and -
		"\nSelecione textos que come�am com n�meros e que usam ponto como separador decimal, e <ENTER> para concluir: "
	      )
	i     0
	total 0.00
  )

  ;; Sum
  (if ss
    (progn
      (while
	(setq ent (ssname ss i))
	 (setq value (atof (cdr (assoc 1 (entget ent)))) ; NOTE: only leading numberic chars.
	       total (+ total value)
	       i     (1+ i)
	 )
      )

      ;; Text height
      ;; 	get it from the last entity
      (setq height (cdr (assoc 40 (entget (ssname ss (1- i))))))

      ;; Prompt the user
      (setq str_total (rtos total))
      (prompt (strcat "\nValor total: " str_total "."))

      ;; Insert the text
      (setvar "OSMODE" 0)
      (if
	(setq ptins (getpoint
		      " Clique para inserir o texto com o valor total ou <sair>: "
		    )
	)
	 (ad:text nil "_mc" ptins height 0 str_total)
      )
    )
    (prompt "\nNenhum texto num�rico foi selecionado.")
  )

  (ad:endcmd)
  (princ)
)


;;; COMMAND: Get area from certain likely-closed entities

(defun c:vaa (/ area ent ptins)
  (prompt "\nVAA - Obter �rea")
  (ad:inicmd)

  ;; User input
  (if
    (setq ent (car (entsel "\nSelecione uma polilinha, c�rculo ou hachura: ")))
     (if
       (member (cdr (assoc 0 (entget ent))) '("LWPOLYLINE" "CIRCLE" "HATCH"))
	(progn
	  (command "_area" "_o" ent)
	  (setvar "DIMZIN" 0)
	  (setq area (rtos (getvar "AREA")))

	  ;; Prompt the user
	  (prompt (strcat "\n�rea: " area "."))

	  ;; Insert the text
	  (setvar "OSMODE" 0)
	  (if
	    (setq
	      ptins (getpoint " Clique para inserir o texto com a �rea ou <sair>: ")
	    )
	     (ad:text nil "_mc" ptins (* *ad:th* *ad:sc*) 0 area)
	  )
	)

	;; Invalid entity
	(prompt "\nObjeto inv�lido! Precisa ser uma POLILINHA, C�RCULO ou HACHURA.")
     )
  )

  (ad:endcmd)
  (princ)
)


;;; COMMAND: Get angle from linear entities

(defun c:va (/ ang pt ptins str_ang tip)
  (prompt "\nVA - Obter �ngulo de objeto linear")
  (ad:inicmd)

  ;; User input
  (setvar "OSMODE" 512)
  (setq pt (getpoint "\nClique sobre uma entidade linear: "))
  (setq ang (ad:angle_pt pt))

  ;; Prompt the user
  (if (= 0 (getvar "AUPREC"))		; precision
    (setq tip " Dica: Para formato e casas decimais, use _UNITS.") ; tip
    (setq tip "")
  )
  (setvar "DIMZIN" 0)
  (setq str_ang (strcat (angtos (ad:iang ang)) " ou " (angtos ang)))
  (prompt (strcat "\n�ngulos nos dois sentidos: " str_ang "." tip))

  ;; Insert text
  (setvar "OSMODE" 0)
  (if
    (setq
      ptins (getpoint " Clique para inserir o texto com os �ngulos ou <sair>: ")
    )
     (ad:text nil
	      "_mc"
	      ptins
	      (* *ad:th* *ad:sc*)
	      (angtos ang)
	      (strcat "< " str_ang " >")
     )
  )

  (ad:endcmd)
  (princ)
)


;;; COMMAND: Get angle by vertex

(defun c:vv (/ ang1 ang2 ang_in ang_out pt1 pt1a pt2 pt2a ptins ptint str_ang)
  (prompt "\nVV - Obter �ngulo de v�rtice")
  (ad:inicmd)

  ;; User input
  (setvar "OSMODE" 512)
  (if
    (and
      (setq pt1 (getpoint "\nClique sobre a primeira linha: "))
      (setq pt2 (getpoint "\nClique sobre a segunda linha: "))
    )
     (progn
       (setvar "OSMODE" 0)
       (setq pt1a (osnap (polar pt1 0.00 0.01) "_nea")
	     pt2a (osnap (polar pt2 0.00 0.01) "_nea")
       )

       ;; Fix if vertical
       (if (= 0.0 (distance pt1 pt1a))
	 (setq pt1a (osnap (polar pt1 (/ pi 2) 0.01) "_nea"))
       )
       (if (= 0.0 (distance pt2 pt2a))
	 (setq pt2a (osnap (polar pt2 (/ pi 2) 0.01) "_nea"))
       )

       ;; Intersection
       (if
	 (setq ptint (inters pt1 pt1a pt2 pt2a nil))
	  (progn

	    ;; Prompt the user
	    (setq ang1 (angle ptint pt1)
		  ang2 (angle ptint pt2)
	    )
	    (prompt (strcat "\n�ngulo 1: " (angtos ang1) ". �ngulo 2: " (angtos ang2) "."))
	    (setq ang_in  (abs (- ang1 ang2))
		  ang_out (- (* 2 pi) ang_in)
	    )
	    (setq str_ang
		   (strcat (angtos (max ang_in ang_out))
			   " \U+2220 "
			   (angtos (min ang_in ang_out))
		   )
	    )
	    (prompt (strcat "\n�ngulos replementares: " str_ang "."))

	    ;; Insert text
	    (setvar "OSMODE" 0)
	    (if
	      (setq ptins (getpoint " Clique para inserir o texto com os �ngulos ou <sair>: "))
	       (ad:text nil "_mc" ptins (* *ad:th* *ad:sc*) 0 str_ang) ; TO-DO: bisect for rot
	    )
	  )
	  (prompt "\nN�o h� �ngulo.")
       )
     )
     (prompt "\nRequer dois pontos sobre um objeto linear.")
  )

  (ad:endcmd)
  (princ)
)


;;; COMMAND: Get length of LWPOLYLINE

(defun c:vpl (/ entlist len ptins rot sel)
  (prompt "\nVPL - Obter comprimento de polilinha aberta")
  (ad:inicmd)

  ;; User input
  (if
    (setq sel (entsel "\nSelecione uma polilinha aberta para medir: "))
     (progn
       (setq entlist (entget (car sel)))
       (if
	 (and
	   (= (cdr (assoc 0 entlist)) "LWPOLYLINE")
	   (member (cdr (assoc 70 entlist)) '(0 128)) ; open LWPL
	 )

	  ;; Prompt the user
	  (progn
	    (setq len (rtos (ad:pllen (car sel))))
	    (prompt (strcat "\nComprimento: " len "."))

	    ;; Insert the text
	    (setvar "OSMODE" 0)
	    (if
	      (setq
		ptins (getpoint
			" Clique para inserir o texto com o comprimento ou <sair>: "
		      )
	      )
	       (progn
		 (setq rot (angtos (ad:angle_pt (osnap (cadr sel) "_nea"))))
		 (ad:text nil "_mc" ptins (* *ad:th* *ad:sc*) rot len)
	       )
	    )
	  )

	  ;; Invalid entity
	  (prompt "\nObjeto inv�lido! Precisa ser uma POLILINHA aberta.")
       )
     )
  )

  (ad:endcmd)
  (princ)
)

;;; EOF
