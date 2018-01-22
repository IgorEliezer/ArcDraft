;;;; g_layer.lsp
;;;; General commands for layer handling.
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2018-01-22
;;;; License: ArcDraft, see LICENSE.txt.


;;;; Set up commands for layer handling.


;;; ---- COMMANDS ----

;;; COMMAND: Set current layer by selection

(defun c:cur (/ entname layname)
  (prompt "\nCUR - Definir camada corrente por seleção")
  (ad:inicmd)

  (setq	entname	(car (entsel "\nSelecione um objeto para definir camada corrente: "))
	layname	(cdr (assoc 8 (entget entname)))
  )
  (command "_layer" "_s" layname "")
  (prompt (strcat "\nA camada \"" layname "\" tornou-se corrente."))

  (ad:endcmd)
  (princ)
)


;;; COMMAND: Create layer and set it current

(defun c:nla (/ laycolor layname)
  (prompt "\nNLA - Criar camada")
  (ad:inicmd)

  (setq	layname	 (getstring T "\nNome da camada a criar: ")
	laycolor (getstring "\nDigite a cor da camada a criar (número de 0 a 255): ")
  )

  (command "_layer" "_make" layname "_color" laycolor "" "")
  (command "_color" "_bylayer")
  (prompt
    (strcat "A camada \"" layname "\" na color " laycolor " foi criada e está corrente.")
  )

  (ad:endcmd)
  (princ)
)

;;; EOF