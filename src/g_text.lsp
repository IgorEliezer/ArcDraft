;;;; g_text.lsp
;;;; General commands for texts.
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2018-01-22
;;;; License: ArcDraft, see LICENSE.txt.


;;;; Set up commands for text (and blocks, partially) editing.


;;; ---- COMMANDS ----

;;; COMMAND: Align texts and blocks

(defun c:ae (/ ang entlist ent ent_internal ent_last msg pt1 sel)
  (prompt "\nAE - Alinha texto e bloco sem atributo com objeto linear")
  (ad:inicmd)

  ;; Point for alignment angle
  (setvar "OSMODE" 512)			; nea
  (if
    (setq
      pt1
       (getpoint
	 "\nClique numa linha para obter um ângulo de alinhamento, ou no vazio para zero graus: "
       )
    )
     (progn

       ;; Alignment angle
       (if
	 (null (setq ang (ad:angle_pt pt1))) ; check if on empty space
	  (setq ang 0.00)		; set alignment angle to zero
       )
       (prompt (strcat "\nÂngulo obtido: " (angtos ang 0 2) "°."))

       ;; Entity to be aligned
       (setq msg "\nClique sobre um texto, bloco sem atributo ou atributo para alinhar: ")
       (while
	 (setq sel (entsel msg))	; selection

	  ;; Check entity
	  (setq ent (car sel))
	  (if
	    (member (cdr (assoc 0 (entget ent)))
		    '("TEXT" "MTEXT" "ATTDEF" "INSERT")

	    )				; valid entity types

	     ;; then: valid, proceed...
	     (progn

	       ;; Entity list
	       (setq ent_internal (car (nentselp (cadr sel))))
					; try to get the subentity by selection point
	       (if
		 (= (cdr (assoc 0 (entget ent_internal))) "ATTRIB")
					; subentity is a block attribute
		  (setq entlist (entget ent_internal)) ; then: from subentity
		  (setq entlist (entget ent)) ; else: from entity
	       )

	       ;; Rotate object if clicking again
	       (if (eq ent ent_last)	; if it is the same entity as the previous
		 (setq ang (ad:fixangle (+ ang pi))) ; add 1 pi to the rotation angle
	       )

	       ;; Modify the entity
	       (entmod (subst (cons 50 ang) (assoc 50 entlist) entlist))
	       (entupd ent)		; update entity

	       ;; Record last entity and change message for new selections
	       (setq ent_last ent
		     msg "\nClique sobre um texto, bloco sem atributo ou atributo para alinhar (clique no mesmo para inverter): "
	       )
	     )

	     ;; else: invalid, prompt the user.
	     (prompt "\nElemento inválido!")
	  )
       )
     )
  )

  (ad:endcmd)
  (princ)
)


;;; COMMAND: Insert note with leader

(defun c:nota (/ j-mode justify pt1 pt2 pt3 ptins)
  (prompt "\nNOTA - Nota com chamada")
  (ad:inicmd)

  ;; Status prompt
  (if
    (= *ad:nota_j* 0)
     (setq j-mode "centralizado")
     (setq j-mode "justificado")
  )
  (prompt
    (strcat "\nAltura de texto: " (rtos (* *ad:th* *ad:sc*)) ". Texto: " j-mode ".")
  )

  ;; Leader

  (setvar "ORTHOMODE" 0)
  (if
    (and
      (setq pt1 (getpoint "\nEspecifique o primeiro ponto: "))
      (setq pt2 (getpoint pt1 "\nEspecifique o segundo ponto: "))
    )
     (progn
       (command	"_pline"
		pt1
		pt2
		(progn
		  (setvar "ORTHOMODE" 1)
		  (setq pt3 (getpoint pt2 "\nEspecifique o terceiro ponto: "))
		)
		(if pt3
		  (command "")		; finish polyline
		)
       )

       ;; Text
       (if (and pt2 pt3)
	 (progn
	   (if (= *ad:nota_j* 0)
	     (setq ptins   (ad:ptmed pt2 pt3)
		   justify "_bc"
	     )
	     (if (<= (car pt2) (car pt3)) ; if rightward
	       (setq ptins   (polar pt2 0.0 (* *ad:th* *ad:sc*))
		     justify "_bl"
	       )
	       (setq ptins   (polar pt2 pi (* *ad:th* *ad:sc*))
		     justify "_br"
	       )
	     )
	   )
	   (prompt "\nEscreva a nota (ENTER para pular linha, 2 ENTERs para sair): ")
	   (command "_dtext" justify ptins (* *ad:th* *ad:sc*) 0.0) ; let user type
	 )
       )
     )
  )

  (ad:endcmd)
  (princ)
)


;;; COMMAND: Text numbering

(defun c:ns (/ ent entlist entlist_assoc1 num prefix str sufix)
  (prompt "\nNS - Numerador sequencial")
  (ad:inicmd)

  ;; user input
  (if
    (setq num (getint "\nDigite o número de partida: "))
     (progn
       (setq prefix (getstring "\nDigite o prefixo ou <ENTER> para nada: ")
	     sufix  (getstring "\nDigite o sufixo ou <ENTER> para nada: ")
       )

       ;; user selection
       (while
	 (setq
	   ent (car (nentsel "\nSelecione um texto ou <sair>: "))
	 )

	  ;; get data
	  (setq entlist (entget ent))
	  (setq entlist_assoc1 (assoc 1 entlist))

	  ;; construct text and pair, then insert and modify
	  (setq str (strcat prefix (itoa num) sufix))

	  (entmod (subst (cons 1 str) entlist_assoc1 entlist))
	  (setq num (1+ num))
       )
     )
  )

  (ad:endcmd)
  (princ)
)


;;; COMMAND: Sum numeric values from texts

(defun c:somt (/ ent h i ptins ss str_total total value)
  (prompt "\nSOMT - Somar valores numéricos de textos")
  (ad:inicmd)

  ;; User input
  (setq	ss    (ad:ssgetp
		'((0 . "TEXT") (1 . "#*,[+]#*,[`-]#*")) ; also accepts + and -
		"\nSelecione textos que começam com números e que usam ponto como separador decimal, e <ENTER> para concluir: "
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
      (setq h (cdr (assoc 40 (entget (ssname ss (1- i))))))

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
	 (ad:text str_total "_mc" ptins h nil)
      )
    )
    (prompt "\nNenhum texto numérico foi selecionado.")
  )

  (ad:endcmd)
  (princ)
)


;;; COMMAND: Copy and transfer text values
;;; 	TO-DO: clean mtext value.

(defun c:tt (/ ent1 ent1_assoc1 ent2 ent2_assoc1 entlist2)
  (prompt "\nTT - Transfere texto de textos, multitextos e atributos de blocos")
  (ad:inicmd)

  ;; user input
  (while
    (and
      (setq ent1 (car (nentsel "\nSelecione o objeto de origem: ")))
      (setq ent2 (car (nentsel "\nSelecione o objeto de destino: ")))
    )

     ;; get data
     (setq ent1_assoc1 (assoc 1 (cdr (entget ent1)))
	   entlist2    (entget ent2)
	   ent2_assoc1 (assoc 1 entlist2)
     )

     ;; replace
     (entmod (subst ent1_assoc1 ent2_assoc1 entlist2))
  )

  (ad:endcmd)
  (princ)
)


;;; COMMAND: Unite texts

(defun c:unt (/ ent1 ent2 entlist1 entlist2 str_1 str_2 str_new str_new_dxf)
  (prompt "\nUNT - Unir textos")
  (ad:inicmd)

  ;; element 1
  (setvar "OSMODE" 0)
  (setq ent1 (car (entsel "\nSelecione o texto a ser completado: ")))

  ;; text
  (while
    (setq ent2 (car (entsel "\nSelecione o texto a adicionar: ")))
     (progn
       ;; anchor text
       (setq entlist1 (entget ent1))
       (setq str_1 (cdr (assoc 1 entlist1)))

       ;; complementary text
       (setq entlist2 (entget ent2))
       (setq str_2 (cdr (assoc 1 entlist2)))

       ;; new text
       (setq str_new (strcat str_1 " " str_2))

       ;; replace and update
       (setq str_new_dxf (cons 1 str_new))
       (entmod (subst str_new_dxf (assoc 1 entlist1) entlist1))
     )
  )

  ;; delete old object
  (if (null *ad:unt:delete*) ; TO:DO: implement global var.
    (entdel ent2)
  )

  (ad:endcmd)
  (princ)
)

;;; EOF
