;;;; g_measure.lsp
;;;; General commands for measurements, reading and math.
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2018-03-27
;;;; License: ArcDraft, see LICENSE.txt.


;;;; Set up commands for text parsing and math.


;;; ---- FUNCTIONS ----


;;; ---- COMMANDS ----

;;; COMMAND: Sum numeric values from texts

(defun c:somt (/ ent height i ptins ss stlname str_total total value)
  (prompt "\nSOMT - Soma valores num�ricos de textos")
  (ad:inicmd)

  ;; User input
  (setq	ss    (ad:ssgetp
		'((0 . "TEXT") (1 . "#*,[+]#*,[`-]#*")) ; also accepts + and -
		"\nSelecione textos que come�am com n�meros e que usam ponto como separador decimal. <ENTER> para concluir: "
	      )
	i     0
	total 0.00
  )

  ;; Sum
  (while
    (setq ent (ssname ss i))
     (setq value     (atof (cdr (assoc 1 (entget ent)))) ; NOTE: only leading numberic chars.
	   total     (+ value total)
	   str_total (rtos total 2 2)
	   i	     (1+ i)
     )
  )

  ;; Text height
  ;; 	get it from the last entity
  (setq height (cdr (assoc 40 (entget (ssname ss (1- i)))))) ; go back

  ;; Prompt the user
  (setq str_total (rtos total 2 2))
  (prompt (strcat "\nValor total: " str_total "."))

  ;; Insert the text
  ;;	TO-DO: let user configure insert height
  (setvar "OSMODE" 0)			; turn off OSMODE
  (setq stlname (getvar "TEXTSTYLE"))
  (if
    (setq ptins (getpoint " Clique para inserir o texto com o valor total: "))
     (if
       (= (cdr (assoc 40 (tblsearch "STYLE" stlname))) 0.0) ; zero as height?
	(command "_text" "_s" stlname "_j" "_mc" ptins height 0 str_total)
	(command "_text" "_s" stlname "_j" "_mc" ptins 0 str_total)
     )
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
	  (setq area (rtos (getvar "AREA")))

	  ;; Prompt the user
	  (prompt (strcat "\n�rea: " area "."))

	  ;; Insert the text
	  ;; 	TO-DO: (setvar "DIMZIN" 0) to stop zero-suppression
	  (setvar "OSMODE" 0)		; turn off OSMODE
	  (if
	    (setq ptins (getpoint " Clique para inserir o texto com a �rea: "))
	     (command "_text" "_j" "_mc" ptins 1.0 0.0 area) ; hardcoded height
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
  (setq str_ang (strcat (angtos (ad:iang ang)) " ou " (angtos ang)))
  (prompt (strcat "\n�ngulos nos dois sentidos: " str_ang "." tip))

  ;; Insert text
  ;; 	TO-DO: (setvar "DIMZIN" 0) to stop zero-suppression
  (setvar "OSMODE" 0)
  (if
    (setq ptins (getpoint " Clique para inserir o texto com os �ngulos: "))
     (command "_text" "_j" "_mc" ptins 1.0 (angtos ang) (strcat "< " str_ang " >"))
					; hardcoded height
  )

  (ad:endcmd)
  (princ)
)


;;; COMMAND: Get angle by vertex
;;; 	TO-DO: catch error if the user clicks on empty space, which causes ptNa to be nil.
;;;		Error at (inters <args>) line.

(defun c:vv (/ ang1 ang2 ang_in ang_out pt1 pt1a pt2 pt2a ptins ptint str_ang)
  (prompt "\nVV - Obter �ngulo de v�rtice")
  (ad:inicmd)

  ;; User input
  (setvar "OSMODE" 512)
  (if (setq pt1 (getpoint "\nClique sobre a primeira linha: "))
    (if	(setq pt2 (getpoint "\nClique sobre a segunda linha: "))
      (progn
	(setvar "OSMODE" 0)
	(setq pt1a (osnap (polar pt1 0.00 0.01) "_nea")
	      pt2a (osnap (polar pt2 0.00 0.01) "_nea")
	)
	(if
	  (setq ptint (inters pt1 pt1a pt2 pt2a nil)) ; TO-DO: catch error here
	   (progn

	     ;; Prompt the user
	     (setq ang1	(angle ptint pt1)
		   ang2	(angle ptint pt2)
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
	       (setq ptins (getpoint " Clique para inserir o texto com os �ngulos: "))
		(command "_text" "_j" "_mc" ptins 1.0 0 str_ang) ; hardcoded height
	     )
	   )
	   (prompt "\nN�o h� �ngulo.")
	)
      )
    )
  )
  (ad:endcmd)
  (princ)
)


;;; EOF
