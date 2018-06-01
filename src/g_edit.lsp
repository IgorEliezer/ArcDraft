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


;;; COMMAND: Preset OSMODE (BROKEN)
;;; 	NOTICE 1: This command will not work because OSMODE is protected by ad:inicmd.
;;; 	NOTICE 2: Not included in the help.
;;; 	TO-DO: Move it to a proper module.
;;; 	TO-DO: Implement config system.

(defun c:osm (/ var)
  (prompt "OSM - OSNAP pré-definido")
  (ad:inicmd)

  (setq var 679)			; set value: end, mid, cen, int, per, nea
  (setvar "OSMODE" var)
  (prompt "\nRedefinido para os pontos mais usados: end, mid, cen, int, per e nea.")

  (ad:endcmd)
  (princ)
)


;;; COMMAND: Scale multiple

;;; - Function: Dynamic prompt message
;;; 	TO-DO: Move it to a proper module.

(defun ad:msg (msg var)

  ;; If it is a number
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


;;; - Command
;;;	Introduces global *ad:scalefactor*
;;; 	TO-DO: Move it to a proper module.

(defun c:scm (/ pt sc ss)
  (prompt "SCM - Scale múltiplo")
  (ad:inicmd)

  ;;; Check global and use it 
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
