;;;; f_command.lsp
;;;; Command functions.
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2018-07-22
;;;; License: ArcDraft, see LICENSE.txt.


;;;; Basic command functions for default values and prompts.


;;; ---- FUNCTIONS ----

;;; FUNCTION: Set default values

(defun ad:defaults ()

  ;; Working scale (global *ad:sc*)
  (if (null *ad:sc*)
    (setq *ad:sc* 0.1)			; default 1:100
  )
  (setvar "DIMZIN" 8)			; suppress trailing zeros
  (prompt (strcat " Escala de trabalho: 1:" (rtos (* 1000 *ad:sc*)) ".")) ; resumes last prompt

  ;; Text height (global *ad:th*)
  (if (null *ad:th*)
    (setq *ad:th* 2.0)			; default height
  )
  (setvar "DIMZIN" 0)			; includes trailing zeros
  (prompt (strcat " Altura base de texto: " (rtos *ad:th*) "."))

  ;; Coord format (global *ad:coord_f*)
  (if (null *ad:coord_f*)
    (setq *ad:coord_f* "XY")		; default X,Y format
  )
  (prompt (strcat " Coordenadas: " *ad:coord_f* "."))

  ; Note justify (global *ad:nota_j*)
  (if (null *ad:nota_j*)
    (setq *ad:nota_j* 0)		; default centered (1 = align left/right)
  )
  (prompt (strcat " Nota: " "centralizada" ".")) ; TO-DO: hardcoded string
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


;;; EOF
