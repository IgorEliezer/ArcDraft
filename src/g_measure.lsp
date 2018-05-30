;;;; g_measure.lsp
;;;; General commands for measurements, reading and math.
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2018-03-27
;;;; License: ArcDraft, see LICENSE.txt.


;;;; Set up commands for block listing, text parsing and math.


;;; ---- COMMANDS ----

;;; COMMAND: Insert coordenates

(defun c:ico (/ coord pref_x pref_y pt1 pt2 pta ptins x y)
  (prompt "\nICO - Inserir coordenadas X e Y")
  (ad:inicmd)

  ;; User input
  (setq	pt1 (getpoint "\nClique num ponto para obter as coordenadas: ")
	pt2 (getpoint pt1 "\nClique no segundo ponto para linha de chamada: ")
  )

  ;; Build string
  (setq	x      (rtos (car pt1))
	y      (rtos (cadr pt1))
	pref_x "X = "
	pref_y "Y = "
  )
  (setq coord (strcat pref_x x " ; " pref_y y))

  ;; 3rd point for leader line
  (if (<= (car pt1) (car pt2))		; if rightward
    (setq pta (append (list (+ (car pt2) (* (strlen coord) 0.66))) (cdr pt2)))
    (setq pta (append (list (- (car pt2) (* (strlen coord) 0.66))) (cdr pt2)))
  )

  ;; Draw leader line
  (command "_pline" pt1 pt2 pta "")

  ;; Insert text
  (setq ptins (ad:ptmed pt2 pta))
  (ad:text nil "_bc" ptins 1.0 nil coord) ; hardcoded height

  (ad:endcmd)
  (princ)
)


;;; COMMAND: List and count blocks from selection
;;; 	TO-DO: Move it to a proper module.

(defun c:lbl (/ div ent header len luprec_or name result ss total)
  (prompt "\nLBL - Listar blocos de uma sele��o")
  (ad:inicmd)

  ;; User input
  (initget 0 "L C")
  (if
    (= (getkword "\nEscolha uma op��o [Listar/Contar de um bloco] <Listar>: ") "C")

     ;; - Count
     (progn
       (if
	 (= (setq name (getstring t "\nDigite o nome do bloco ou <obter de um bloco>: ")) "")
	  (progn
	    (while
	      (/= "INSERT"
		  (cdr (assoc 0 (entget (car (setq ent (entsel "\nSelecione um bloco: "))))))
	      )
	       (prompt "\nEste objeto n�o � um bloco!")
	    )
	    (setq name (cdr (assoc 2 (entget (car ent)))))
	  )
       )
       (setq ss	(ad:ssgetp
		  (list '(0 . "INSERT") (cons 2 name))
		  (strcat " Nome do bloco: \"" name "\".\nSelecione objetos para contar: ")
		)
       )
       (if ss
	 (setq len (sslength ss))
	 (setq len 0)
       )
       (alert (strcat "\nH� " (itoa len) " blocos com o nome \"" name "\" na sele��o.")
       )
     )

     ;; - List
     (progn
       (setq result
	      (ad:block-counter
		(ad:ssgetp '((0 . "INSERT")) "\nSelecione objetos para listar blocos: ")
	      )
       )

       ;; Propmt the user
       (if result
	 (progn
	   (setq total	(strcat "N� de nomes de blocos: " (itoa (length result)))
		 header	"\nNome do bloco (Quantidade)"
		 div	"\n-------------------------------"
	   )

	   ;; Pop-up
	   (setq luprec_or (getvar "LUPREC"))
	   (setvar "LUPREC" 0)
	   (alert (strcat total div header div "\n" (ad:alist->str result " (" ")\n") ")"))
	   (setvar "LUPREC" luprec_or)

	   ;; Command line
	   (prompt "\n\n")
	   (prompt total)
	   (prompt div)
	   (prompt header)
	   (prompt div)
	   (foreach line result
	     (prompt (strcat "\n" (car line) " (" (itoa (cdr line)) ")"))
	   )
	   (prompt div)
	   (prompt "\nPressione F2 para ver resultados.")
	 )
	 (prompt "\nNenhum bloco foi selecionado.")
       )
     )
  )
  (ad:endcmd)
  (princ)
)


;;; COMMAND: List layers and count entities from selection
;;; 	TO-DO: Move it to a proper module.

(defun c:lla (/ div header len luprec_or name result ss total)
  (prompt "\nLLA - Listar camadas de uma sele��o")
  (ad:inicmd)

  ;; User input
  (initget 0 "L C")
  (if
    (= (getkword "\nEscolha uma op��o [Listar/Contar de uma camada] <Listar>: ") "C")

     ;; - Count
     (progn
       (if
	 (= (setq name (getstring t "\nDigite o nome da camada ou <obter de um objeto>: ")) "")
	  (setq name (cdr (assoc 8 (entget (car (entsel "\nSelecione um objeto: "))))))
       )
       (setq ss	(ad:ssgetp
		  (list (cons 8 name))
		  (strcat " Nome da camada: \"" name "\".\nSelecione objetos para contar: ")
		)
       )
       (if ss
	 (setq len (sslength ss))
	 (setq len 0)
       )
       (alert (strcat "\nH� " (itoa len) " objetos na camada \"" name "\" na sele��o.")
       )
     )

     ;; - List
     (progn
       (setq result (ad:layer-counter
		      (ad:ssgetp nil "\nSelecione objetos para listar camadas: ")
		    )
       )

       ;; Propmt the user
       (if result
	 (progn
	   (setq total	(strcat "N� de camadas: " (itoa (length result)))
		 header	"\nNome da camada (Quantidade)"
		 div	"\n-------------------------------"
	   )

	   ;; Pop-up
	   (setq luprec_or (getvar "LUPREC"))
	   (setvar "LUPREC" 0)
	   (alert (strcat total div header div "\n" (ad:alist->str result " (" ")\n") ")"))
	   (setvar "LUPREC" luprec_or)

	   ;; Command line
	   (prompt "\n\n")
	   (prompt total)
	   (prompt div)
	   (prompt header)
	   (prompt div)
	   (foreach line result
	     (prompt (strcat "\n" (car line) " (" (itoa (cdr line)) ")"))
	   )
	   (prompt div)
	   (prompt "\nPressione F2 para ver resultados.")
	 )
	 (prompt "\nNenhum objeto foi selecionado.")
       )
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
		"\nSelecione polilinhas medir (somente as abertas ser�o consideradas), e <ENTER> para concluir: "
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
      ;;	TO-DO: let user configure insert height
      (setvar "OSMODE" 0)
      (if
	(setq ptins (getpoint
		      " Clique para inserir o texto com o valor total, ou <ENTER> para sair: "
		    )
	)
	 (ad:text nil "_mc" ptins 1.0 0 str_total) ; hardcoded height
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
      ;;	TO-DO: let user configure insert height
      (setvar "OSMODE" 0)
      (if
	(setq ptins (getpoint
		      " Clique para inserir o texto com o valor total, ou <ENTER> para sair: "
		    )
	)
	 (ad:text nil "_mc" ptins height 0 str_total)
      )
    )
    (prompt "\nNenhum texto foi selecionado.")
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
	  (setvar "OSMODE" 0)
	  (if
	    (setq
	      ptins (getpoint " Clique para inserir o texto com a �rea, ou <ENTER> para sair: ")
	    )
	     (ad:text nil "_mc" ptins 1.0 0 area) ; hardcoded height
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
    (setq
      ptins (getpoint " Clique para inserir o texto com os �ngulos, ou <ENTER> para sair: ")
    )
     (ad:text nil "_mc" ptins 1.0 (angtos ang) (strcat "< " str_ang " >")) ; hardcoded height
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
	       (setq
		 ptins (getpoint
			 " Clique para inserir o texto com os �ngulos, ou <ENTER> para sair: "
		       )
	       )
		(ad:text nil "_mc" ptins 1.0 0 str_ang)
					; hardcoded height. TO-DO: bisect for rot
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
			" Clique para inserir o texto com o comprimento, ou <ENTER> para sair: "
		      )
	      )
	       (progn
		 (setq rot (angtos (ad:angle_pt (osnap (cadr sel) "_nea"))))
		 (ad:text nil "_mc" ptins 1.0 rot len) ; hardcoded height
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
