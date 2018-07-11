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
	       (if (eq ent ent_last) ; if it is the same entity as the previous
		 (setq ang (ad:fixangle (+ ang pi))) ; add 1 pi to the rotation angle
	       )

	       ;; Modify the entity
	       (entmod (subst (cons 50 ang) (assoc 50 entlist) entlist))
	       (entupd ent)		; update entity

	       ;; Record last entity and change message for new selections
	       (setq ent_last ent
		     msg	  "\nClique sobre um texto, bloco sem atributo ou atributo para alinhar (clique no mesmo para inverter): "
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
       (entmod
	 (subst str_new_dxf (assoc 1 entlist1) entlist1)
       )
     )
  )

  (ad:endcmd)
  (princ)
)


;;; COMMAND: Copy and transfer text values
;;; 	TO-DO: distinguish between mtext vs. dtext.

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
	   entlist2	   (entget ent2)
	   ent2_assoc1 (assoc 1 entlist2)
     )

     ;; replace
     (entmod (subst ent1_assoc1 ent2_assoc1 entlist2))
  )

  (ad:endcmd)
  (princ)
)


;;; COMMAND: Text numbering

(defun c:ns (/ entlist entlist_assoc1 ent num str str_assoc1 str_num str_prefix str_sufix)
  (prompt "\nNS - Numerador sequencial")
  (ad:inicmd)

  ;; user input
  (setq num (getint "\nDigite o número de partida: "))
  (setq	str_prefix (getstring "\nDigite o prefixo ou <ENTER> para nada: ")
	str_sufix  (getstring "\nDigite o sufixo ou <ENTER> para nada: ")
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
     (setq str_num (itoa num)
	   str	   (strcat str_prefix str_num str_sufix)
     )
     (setq str_assoc1 (cons 1 str))

     (entmod (subst str_assoc1 entlist_assoc1 entlist))
     (setq num (1+ num))
  )

  (ad:endcmd)
  (princ)
)

;;; EOF
