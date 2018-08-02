;;;; g_aconfig.lsp
;;;; ArcDraft's config command.
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2018-07-31
;;;; License: ArcDraft, see LICENSE.txt.


;;;; Command for ArcDraft's config.


;;; ---- COMMANDS ----

;;; COMMAND: ArcDraft's basic config

(defun c:aconfig (/ coord_f msg_nota_j msg_sc nota_j option sc th)
  (prompt "\nACONFIG - Configuração básica do ArcDraft")
  (ad:inicmd)

  (initget 0 "E A F N")
  (setq	option
	 (getkword
	   "\nEscolha um item para configurar [Escala de trabalho/Altura de texto/Formato de coordenada/Nota]: "
	 )
  )
  (cond

    ;; Working scale *ad:sc*
    ((= option "E")
     (progn
       (setvar "DIMZIN" 8)
       (setq msg_sc (strcat "1:" (rtos (* 1000.0 *ad:sc*))))
       (if (setq sc (getreal (strcat (ad:msg "\nDigite a nova escala de trabalho" msg_sc) "1:")))
	 (setq *ad:sc* (/ sc 1000.0))
       )
     )
    )

    ;; Text height *ad:th*
    ((= option "A")
     (progn
       (setvar "DIMZIN" 0)		; includes trailing zeros
       (if (setq th (getreal (ad:msg "\nDigite a altura base de texto" (rtos *ad:th*))))
	 (setq *ad:th* th)
       )
     )
    )

    ;; Coord format *ad:coord_f*
    ((= option "F")
     (progn
       (initget 0 "XY NE")
       (if (setq coord_f
		  (getkword (ad:msg "\nEscolha o formato do comando ICO [XY/NE]" *ad:coord_f*))
	   )
	 (setq *ad:coord_f* coord_f)
       )
     )
    )

    ;; Note justification *ad:nota_j*
    ((= option "N")
     (progn
       (if (= *ad:nota_j* 0)
	 (setq msg_nota_j "Centralizado")
	 (setq msg_nota_j "Justificado")
       )
       (initget 0 "Centralizado Justificado")
       (setq nota_j (getkword (ad:msg "\nEscolha o alinhamento do texto da nota [Centralizado/Justificado]" msg_nota_j)))
       (cond
	 ((= nota_j "Centralizado") (setq *ad:nota_j* 0))
	 ((= nota_j "Justificado") (setq *ad:nota_j* 1))
       )       
     )
    )
  )

  (ad:endcmd)
  (princ)
)

;;; EOF
