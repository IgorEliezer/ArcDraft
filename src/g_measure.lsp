;;;; g_measure.lsp
;;;; General commands for measurements, reading and math.
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2018-03-27
;;;; License: ArcDraft, see LICENSE.txt.


;;;; Set up commands for block listing, text parsing and math.


;;; ---- COMMANDS ----

;;; COMMAND: Insert coordenates

(defun c:ico (/)
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
  (if (<= (car pt1) (car pt2))		; left->right?
    (setq pta (append (list (+ (car pt2) (* (strlen coord) 0.5))) (cdr pt2)))
    (setq pta (append (list (- (car pt2) (* (strlen coord) 0.5))) (cdr pt2)))
  )

  ;; Draw leader line
  (command "_pline" pt1 pt2 pta "")

  ;; Insert text
  (setq ptins pta)			; CHANGE IT!
  (ad:text nil "_bc" ptins 1.0 nil coord)

  (ad:endcmd)
  (princ)
)

;;; COMMAND: List and count blocks from selection
;;; 	TO-DO: Move it to a proper module.

(defun c:lbl (/ div header result total)
  (prompt "\nLBL - Listar blocos de uma sele��o")
  (ad:inicmd)

  ;; User input
  (setq	result (ad:block-counter
		 (ad:ssgetp '((0 . "INSERT"))
			    "\nSelecione para listar blocos, e <ENTER> para concluir: "
		 )
	       )
  )

  ;; Propmt the user
  (if result
    (progn
      (setq total  (strcat "N� de nomes de blocos: " (itoa (length result)))
	    header "\nNome do bloco (Quantidade)"
	    div	   "\n-------------------------------"
      )
      (alert (strcat total div header div "\n" (ad:alist->str result " (" ")\n") ")"))
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
    (prompt "\nNenhum bloco selecionado.")
  )

  (ad:endcmd)
  (princ)
)


;;; COMMAND: List layers and count entities from selection
;;; 	TO-DO: Move it to a proper module.

(defun c:lla (/ div header result total)
  (prompt "\nLLA - Listar camadas de uma sele��o")
  (ad:inicmd)

  ;; User input
  (setq	result (ad:layer-counter
		 (ad:ssgetp nil
			    "\nSelecione para listar camadas, e <ENTER> para concluir: "
		 )
	       )
  )

  ;; Propmt the user
  (if result
    (progn
      (setq total  (strcat "N� de camadas: " (itoa (length result)))
	    header "\nNome da camada (Quantidade)"
	    div	   "\n-------------------------------"
      )
      (alert (strcat total div header div "\n" (ad:alist->str result " (" ")\n") ")"))
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
    (prompt "\nNenhum objeto selecionado.")
  )

  (ad:endcmd)
  (princ)
)


;;; COMMAND: Sum numeric values from texts
;;; 	TO-DO: Move it to a proper module.

(defun c:somt (/ ent height i ptins ss stlname str_total total value)
  (prompt "\nSOMT - Soma valores num�ricos de textos")
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
  (setq height (cdr (assoc 40 (entget (ssname ss (1- i))))))

  ;; Prompt the user
  (setq str_total (rtos total 2 2))
  (prompt (strcat "\nValor total: " str_total "."))

  ;; Insert the text
  ;;	TO-DO: let user configure insert height
  (setvar "OSMODE" 0)			; turn off OSMODE
  (setq stlname (getvar "TEXTSTYLE"))	; TO-DO: get the style from the last entity
  (if
    (setq ptins	(getpoint
		  " Clique para inserir o texto com o valor total, ou <ENTER> para sair: "
		)
    )
     (ad:text stlname "_mc" ptins height 0 str_total)
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

;;; EOF
