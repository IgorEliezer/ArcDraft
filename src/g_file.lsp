;;;; g_file.lsp
;;;; General commands for environment/file management.
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2018-01-31
;;;; License: ArcDraft, see LICENSE.txt.


;;;; Commands for file management and handling. For now, a "better" save.


;;; ---- COMMANDS ----

;;; SS - Super save
;;; 	TO-DO: check if layer '0' is off

(defun c:ss ()
  (prompt "\nSS - Super save (purge + zoom + camada 0 corrente)")
  (ad:inicmd)

  ;; Check if layer "0" is editable
  (if
    (< 0 (cdr (assoc 70 (tblsearch "LAYER" "0"))))
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
	  (setvar "clayer" "0")
	  (setvar "cecolor" "bylayer")
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