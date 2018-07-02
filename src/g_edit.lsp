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
       (while
	 (not (member '(0 . "VIEWPORT")
		      (entget (setq ent (car (entsel "\nSelecione um viewport: "))))
	      )
	 )
	  (prompt "\nIsso não é um viewport!")
       )
       (setvar "DIMZIN" 8)		; suppress trailing zeros
       (prompt
	 (strcat " Escala atual: 1:"
		 (rtos (/ 1000 (vla-get-customscale (vlax-ename->vla-object ent))))
		 "."
	 )
       )
       (if
	 (setq sc (getreal "\nDefina a nova escala do viewport (1:___) <sair>: "))

	  ;; Set viewport scale
	  (progn
	    (vla-put-customscale (vlax-ename->vla-object ent) (/ 1000 sc))
	    (prompt (strcat "\nEscala do viewport mudado para 1:" (rtos sc 2) "."))
	  )
       )
     )
     (alert "Você precisa estar em modo paper space para usar o comando.")
  )

  (ad:endcmd)
  (princ)
)


;;; COMMAND: OSMODE preset
;;; 	NOTE: ad:inicmd and ad:endcmd commented out due to overriding setvar "OSMODE".
;;;		- see TO-DO in error.lsp.
;;; 	TO-DO: Move it to a proper module.

(defun c:osp (/ var)
  (prompt "OSP - OSNAP pré-definido/recomendado")
;;;  (ad:inicmd)

  (setq var 679)			; set value: end, mid, cen, int, per, nea
  (setvar "OSMODE" var)
  (prompt
    "\nRedefinido para os pontos: end, mid, cen, int, per e nea."
  )

;;;  (ad:endcmd)
  (princ)
)


;;; COMMAND: Scale multiple
;;;	Introduces global *ad:scalefactor*
;;; 	TO-DO: Move it to a proper module.

(defun c:scm (/ pt sc ss)
  (prompt "SCM - Scale múltiplo")
  (ad:inicmd)

  ;; Check global and use it 
  (if (and (null sc) *ad:scalefactor*)
    (setq sc *ad:scalefactor*)
  )

  ;; Scale factor
  (if
    (null
      (setq sc (getreal (ad:msg "\nEspecifique um fator de escala" *ad:scalefactor*)))
    )
     (setq sc *ad:scalefactor*)
     (setq *ad:scalefactor* sc)
  )

  ;; Apply
  (if sc
    (while (setq ss (ad:ssgetp nil "\nSelecione objetos: "))
      (setq pt (getpoint "\nEspecifique um ponto base: "))
      (command "_scale" ss "" pt sc)
    )
  )

  (ad:endcmd)
  (princ)
)

;;; EOF
