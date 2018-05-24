;;;; g_edit.lsp
;;;; General commands for editing.
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2018-02-03
;;;; License: ArcDraft, see LICENSE.txt.


;;;; General commands for editing. For now, a "better" break.


;;; ---- COMMANDS ----

;;; COMMAND: Break point
;;; 	TO-DO: select once, then multiple breaks with while+getpoint

(defun c:bb (/ entnamept pt1)
  (prompt "\nBB - Quebra no ponto")
  (ad:inicmd)

  (setvar "OSMODE" 544)			; nea + int
  (while
    (setq entnamept (entsel "\nSelecione uma linha: "))
     (setq pt1 (getpoint "\nEspecifique o ponto de quebra: "))
     (command "_break" entnamept "_f" pt1 pt1)
  )

  (ad:endcmd)
  (princ)
)


;;; COMMAND: Set viewport scale

(defun c:evp (/ ent sc)
  (prompt "\nEVP - Escala de Viewport")
  (ad:inicmd)

  ;; Check if in paper space
  (if
    (= (getvar "TILEMODE") 0)
     (progn

       ;; User input
       (setvar "DIMZIN" 8)		; suppress trailing zeros
       (setq ent (car (entsel "\nSelecione um viewport: "))
	     sc	 (getreal "\nDefina a nova escala do viewport (1/XXXX): 1 / ")
       )

       ;; Set viewport scale
       (vla-put-customscale (vlax-ename->vla-object ent) (/ 1000 sc))
       (prompt (strcat "\nEscala do viewport mudado para 1/" (rtos sc 2) "."))
     )
     (alert "Você precisa estar em modo paper space para usar o comando.")
  )

  (ad:endcmd)
  (princ)
)


;;; EOF
