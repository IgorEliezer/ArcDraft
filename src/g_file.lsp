;;;; g_file.lsp
;;;; General commands for environment/file management.
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2018-01-31
;;;; License: ArcDraft, see LICENSE.txt.


;;;; Commands for file management and handling.


;;; ---- COMMANDS ----

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


;;; COMMAND: Cycle zoom

(defun c:czoom (/ ent i ss)
  (prompt "\nCZOOM - Zoom circular")
  (ad:inicmd)

  (setq
    ss (ad:ssgetp
	 nil
	 "\nSelecione objetos para dar zoom em cada (Dica: use \"p\" para seleção anterior): "
       )
  )
  (setq i 0)
  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (command "_zoom" "_o" ent "")
    (sssetfirst nil (ssadd ent))
    (initget 0 "Próximo Sair")
    (if
      (= (getkword "\nTecle ENTER para ir ao próximo ou [Sair] <Próximo>: ") "Sair")
       (exit)
       (setq i (1+ i))
    )
  )
  (prompt "\nChegou no último. ")
  (prompt "Concluído.")

  (ad:endcmd)
  (princ)
)

;;; EOF
