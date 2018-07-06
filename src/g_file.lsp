;;;; g_file.lsp
;;;; General commands for environment/file management.
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2018-01-31
;;;; License: ArcDraft, see LICENSE.txt.


;;;; Commands for file management and handling. For now, a "better" save.


;;; ---- FUNCTIONS ----

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


;;; FUNCTION: Set default working scale
;;;	Global *ad:sc*

(defun ad:setscale ()
  (if (null *ad:sc*)
    (progn
      (setq *ad:sc* 0.1)		; default 1:100
      (setvar "DIMZIN" 8)		; suppress trailing zeros
      (prompt
	(strcat	"\n:: Escala de trabalho foi redefinida para 1:"
		(rtos (* 1000 *ad:sc*))
		"."
	)
      )
    )
  )
)
(ad:setscale)


;;; FUNCTION: Set default text height
;;;	Global *ad:th*

(defun ad:settextheight ()
  (if (null *ad:th*)
    (progn
      (setq *ad:th* 2.0)		; default height
      (setvar "DIMZIN" 0)		; includes trailing zeros
      (prompt
	(strcat "\n:: Altura de texto base: " (rtos *ad:th*) ".")
      )
    )
  )
)
(ad:settextheight)


;;; ---- COMMANDS ----

;;; COMMAND: Set working scale

(defun c:est (/ msg_sc sc)
  (prompt "\nEST - Escala de trabalho")
  (ad:inicmd)

  ;; Build numerical scale
  (setvar "DIMZIN" 8)
  (setq msg_sc (strcat "1:" (rtos (* 1000.0 *ad:sc*))))

  ;; User input
  (if
    (setq sc (getreal (strcat (ad:msg "\nDefina a nova escala de trabalho" msg_sc) "1:")))
     (setq *ad:sc*
	    (/ sc
	       1000.0
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
       "\nA camada '0' est� congelada ou trancada. Ela precisa estar edit�vel para o comando funcionar."
     )

     ;; Check if not in block editor mode
     (if
       (/= (getvar "BLOCKEDITOR") 0)
	(prompt
	  "\nVoc� est� no BlockEditor. Precisa sair da edi��o de bloco para poder salvar."
	)

	;; Execution
	(progn
	  (if
	    (< (cdr (assoc 62 (entget (tblobjname "LAYER" "0")))) 0) ; if turned off
	     (alert "A camada '0' est� desligada e ficar� corrente.")
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
