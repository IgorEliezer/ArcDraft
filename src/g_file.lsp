;;;; g_file.lsp
;;;; General commands for environment/file management.
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2018-01-31
;;;; License: ArcDraft, see LICENSE.txt.


;;;; Commands for file management and handling. For now, a "better" save.


;;; ---- FUNCTIONS ----

;;; FUNCTION: Set default values

(defun ad:defaults ()

  ;; Working scale (global *ad:sc*)
  (if (null *ad:sc*)
    (setq *ad:sc* 0.1)
  )					; default 1:100
  (setvar "DIMZIN" 8)			; suppress trailing zeros
  (prompt (strcat " Escala de trabalho: 1:" (rtos (* 1000 *ad:sc*)) ".")) ; resumes last prompt

  ;; Text height (global *ad:th*)
  (if (null *ad:th*)
    (setq *ad:th* 2.0)
  )					; default height
  (setvar "DIMZIN" 0)			; includes trailing zeros
  (prompt (strcat " Altura base de texto: " (rtos *ad:th*) "."))
)
(ad:defaults)				; execute


;;; FUNCTION: Dynamic prompt message generator

(defun ad:msg (msg var)
  (if (numberp var)
    (progn
      (setvar "DIMZIN" 1)		; include leading zeros 0.X
      (setq var (rtos var))		; convert it to string
    )
  )

  ;; Build prompt
  (if var
    (strcat msg " <" var ">: ")
    (strcat msg ": ")
  )
)


;;; ---- COMMANDS ----

;;; COMMAND: ArcDraft's basic config

(defun c:aconfig (/ msg_sc option sc th)
  (prompt "\nACONFIG - Configuração básica do ArcDraft")
  (ad:inicmd)

  (initget 0 "E A")
  (setq	option
	 (getkword
	   "\nEscolha um item para configurar [Escala de trabalho/Altura de texto]: "
	 )
  )
  (cond

    ;; Working scale
    ((= option "E")
     (progn
       (setvar "DIMZIN" 8)
       (setq msg_sc (strcat "1:" (rtos (* 1000.0 *ad:sc*))))
       (if (setq sc (getreal (strcat (ad:msg "\nDefina a nova escala de trabalho" msg_sc) "1:")))
	 (setq *ad:sc* (/ sc 1000.0))
       )
     )
    )

    ;; Text height
    ((= option "A")
     (progn
       (setvar "DIMZIN" 0)		; includes trailing zeros
       (if (setq th (getreal (ad:msg "\nDefina altura base de texto" (rtos *ad:th*))))
	 (setq *ad:th* th)
       )
     )
    )
  )

  (ad:endcmd)
  (princ)
)


;;; COMMAND: Super save

(defun c:ss ()
  (prompt "\nSS - Super save (purge + zoom + camada 0 corrente)")
  (ad:inicmd)

  ;; Check if layer "0" is editable
  (if
    (> (cdr (assoc 70 (tblsearch "LAYER" "0"))) 0)
     (prompt
       "\nA camada '0' está congelada ou trancada. Ela precisa estar editável para o comando funcionar."
     )

     ;; Check if not in block editor mode
     (if
       (/= (getvar "BLOCKEDITOR") 0)
	(prompt
	  "\nVocê está no BlockEditor. Precisa sair da edição de bloco para poder salvar."
	)

	;; Execution
	(progn
	  (if
	    (< (cdr (assoc 62 (entget (tblobjname "LAYER" "0")))) 0) ; if turned off
	     (alert "A camada '0' está desligada e ficará corrente.")
	  )
	  (setvar "CLAYER" "0")
	  (setvar "CECOLOR" "bylayer")
	  (command "_zoom" "_e")
	  (repeat 3 (command "_purge" "_a" "" "_n")) ; 3 full purges (no RegApp)
	  (command "_qsave")
	  (prompt "\nSalvo e limpo! Tecle F2 para ver itens eliminados.\n")
	)
     )
  )

  (ad:endcmd)
  (princ)
)

;;; EOF
