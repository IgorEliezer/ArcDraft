;;;; g_measure.lsp
;;;; General commands for measurements, reading and math.
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2018-03-27
;;;; License: ArcDraft, see LICENSE.txt.


;;;; Set up commands for measurements, text parsing and math.


;;; ---- COMMANDS ----

;;; COMMAND: Insert coordenates

(defun c:ico (/ coord coord2 coord_len msg pref_x pref_y pt1 pt2 pta ptins x y)
  (prompt "\nICO - Inserir coordenadas X e Y")
  (prompt (strcat "\nConfigurações atuais: Formato de coordenada="
		  *ad:coord_f*
		  ", Altura de texto="
		  (rtos (* *ad:th* *ad:sc*))
	  )
  )
  (ad:inicmd)

  ;; User input
  (if
    (setq pt1 (getpoint "\nClique num ponto para obter as coordenadas: "))
     (progn

       ;; Build string
       (setq x (rtos (car pt1))
	     y (rtos (cadr pt1))
       )
       (cond
	 ((= *ad:coord_f* "XY")
	  (progn
	    (setq pref_x "X = "
		  pref_y "Y = "
		  coord	 (strcat pref_x x "; " pref_y y)
		  msg	 (strcat "XY" ": " x "," y ".")
	    )
	  )
	 )
	 ((= *ad:coord_f* "NE")
	  (progn
	    (setq pref_x "E = "
		  pref_y "N = "
		  coord	 (strcat pref_y y "; " pref_x x)
		  msg	 (strcat "NE" ": " y "," x ".")
	    )
	  )
	 )
	 ((= *ad:coord_f* "X-Y")
	  (progn
	    (setq pref_x "X = "
		  pref_y "Y = "
		  coord	 (strcat pref_x x)
		  coord2 (strcat pref_y y)
		  msg	 (strcat "XY" ": " x "," y ".")
	    )
	  )
	 )
       )

       ;; Prompt the user
       (prompt (strcat "\nCoordenadas " msg))

       ;; Leader
       (setvar "OSMODE" 0)
       (if
	 (setq
	   pt2 (getpoint
		 pt1
		 " Clique no segundo ponto para linha de chamada com o valor ou <sair>: "
	       )
	 )
	  (progn

	    ;; Find 3rd point for leader line
	    (setq coord_len (* (strlen coord) 0.7 *ad:th* *ad:sc*)) ; 0.7 is char width
	    (if	(<= (car pt1) (car pt2)) ; if rightward
	      (setq pta (append (list (+ (car pt2) coord_len)) (cdr pt2)))
	      (setq pta (append (list (- (car pt2) coord_len)) (cdr pt2)))
	    )

	    ;; Draw leader line
	    (command "_pline" pt1 pt2 pta "")

	    ;; Insert text
	    (setq ptins (ad:ptmed pt2 pta))
	    (ad:text coord "_bc" ptins (* *ad:th* *ad:sc*) nil)
	    (if	coord2
	      (ad:text coord2
		       "_tc"
		       (polar ptins (* 1.5 pi) (* 0.25 *ad:th* *ad:sc*))
		       (* *ad:th* *ad:sc*)
		       nil
	      )
	    )
	  )
       )
     )
  )

  (ad:endcmd)
  (princ)
)


;;; COMMAND: Sum LWPOLYLINEs

(defun c:sompl (/ ent i len ptins ss str_total total)
  (prompt "\nSOMPL - Somar comprimentos de polilinhas abertas")
  (prompt (strcat "\nConfigurações atuais: Altura de texto="
		  (rtos (* *ad:th* *ad:sc*))
	  )
  )
  (ad:inicmd)

  ;; User input
  (setq	ss    (ad:ssgetp
		'((0 . "LWPOLYLINE") (-4 . "<or") (70 . 0) (70 . 128) (-4 . "or>")) ; open LWPL
		"\nSelecione polilinhas para medir (somente as abertas serão consideradas), e <ENTER> para concluir: "
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
	 (ad:text str_total "_mc" ptins (* *ad:th* *ad:sc*) nil)
      )
    )
    (prompt "\nNenhuma polilinha aberta foi selecionada.")
  )

  (ad:endcmd)
  (princ)
)


;;; COMMAND: Get area from certain likely-closed entities

(defun c:vaa (/ area ent ptins)
  (prompt "\nVAA - Obter área")
  (prompt (strcat "\nConfigurações atuais: Altura de texto="
		  (rtos (* *ad:th* *ad:sc*))
	  )
  )
  (ad:inicmd)

  ;; User input
  (if
    (setq ent (car (entsel "\nSelecione uma polilinha, círculo ou hachura: ")))
     (if
       (member (cdr (assoc 0 (entget ent))) '("LWPOLYLINE" "CIRCLE" "HATCH"))
	(progn
	  (command "_area" "_o" ent)
	  (setvar "DIMZIN" 0)
	  (setq area (rtos (getvar "AREA")))

	  ;; Prompt the user
	  (prompt (strcat "\nÁrea: " area "."))

	  ;; Insert the text
	  (setvar "OSMODE" 0)
	  (if
	    (setq ptins (getpoint " Clique para inserir o texto com a área ou <sair>: "))
	     (ad:text area "_mc" ptins (* *ad:th* *ad:sc*) nil)
	  )
	)

	;; Invalid entity
	(prompt "\nObjeto inválido! Precisa ser uma POLILINHA, CÍRCULO ou HACHURA.")
     )
  )

  (ad:endcmd)
  (princ)
)


;;; COMMAND: Get angle from linear entities

(defun c:va (/ ang pt ptins str_ang tip)
  (prompt "\nVA - Obter ângulo de objeto linear")
  (prompt (strcat "\nConfigurações atuais: Altura de texto="
		  (rtos (* *ad:th* *ad:sc*))
	  )
  )
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
  (prompt (strcat "\nÂngulos nos dois sentidos: " str_ang "." tip))

  ;; Insert text
  (setvar "OSMODE" 0)
  (if
    (setq ptins (getpoint " Clique para inserir o texto com os ângulos ou <sair>: "))
     (ad:text (strcat "< " str_ang " >")
	      "_mc"
	      ptins
	      (* *ad:th* *ad:sc*)
	      (angtos ang)
     )
  )

  (ad:endcmd)
  (princ)
)


;;; COMMAND: Get angle by vertex

(defun c:vv (/ ang1 ang2 ang_in ang_out pt1 pt1a pt2 pt2a ptins ptint str_ang)
  (prompt "\nVV - Obter ângulo de vértice")
  (prompt (strcat "\nConfigurações atuais: Altura de texto="
		  (rtos (* *ad:th* *ad:sc*))
	  )
  )
  (ad:inicmd)

  ;; User input - 1st point
  (setvar "OSMODE" 512)
  (setq pt1 (getpoint "\nClique sobre a primeira linha: "))
  (setvar "OSMODE" 0)
  (if
    (null (setq pt1a (osnap (polar pt1 0.00 0.01) "_nea")))
     (progn
       (prompt "\nVocê deve clicar sobre um objeto linear.")
       (exit)
     )
  )

  ;; User input - 2nd point
  (setvar "OSMODE" 512)
  (setq pt2 (getpoint "\nClique sobre a segunda linha: "))
  (setvar "OSMODE" 0)
  (if
    (null (setq pt2a (osnap (polar pt2 0.00 0.01) "_nea")))
     (progn
       (prompt "\nVocê deve clicar sobre um objeto linear.")
       (exit)
     )
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
       (prompt (strcat "\nÂngulo 1: " (angtos ang1) ". Ângulo 2: " (angtos ang2) "."))
       (setq ang_in  (abs (- ang1 ang2))
	     ang_out (- (* 2 pi) ang_in)
       )
       (setq str_ang
	      (strcat (angtos (max ang_in ang_out))
		      " \U+2220 "
		      (angtos (min ang_in ang_out))
	      )
       )
       (prompt (strcat "\nÂngulos replementares: " str_ang "."))

       ;; Insert text
       (setvar "OSMODE" 0)
       (if
	 (setq ptins (getpoint " Clique para inserir o texto com os ângulos ou <sair>: "))
	  (ad:text str_ang "_mc" ptins (* *ad:th* *ad:sc*) nil)
       )
     )
     (prompt "\nNão há ângulo.")
  )

  (ad:endcmd)
  (princ)
)


;;; COMMAND: Get length of LWPOLYLINE

(defun c:vpl (/ entlist len ptins rot sel)
  (prompt "\nVPL - Obter comprimento de polilinha aberta")
  (prompt (strcat "\nConfigurações atuais: Altura de texto="
		  (rtos (* *ad:th* *ad:sc*))
	  )
  )
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
		 (ad:text len "_mc" ptins (* *ad:th* *ad:sc*) rot)
	       )
	    )
	  )

	  ;; Invalid entity
	  (prompt "\nObjeto inválido! Precisa ser uma POLILINHA aberta.")
       )
     )
  )

  (ad:endcmd)
  (princ)
)


;;; EOF
