;;;; g_measure.lsp
;;;; General commands for measurements, reading and math.
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2018-03-27
;;;; License: ArcDraft, see LICENSE.txt.


;;;; Set up commands for text parsing and math.


;;; ---- FUNCTIONS ----

;;; FUNCTION: SSGET with prompt

(defun ad:ssgetp (filter msg / ss)
  (prompt msg)
  (setvar "nomutt" 1)
  (vl-catch-all-apply '(lambda () (setq ss (ssget filter))))
  (setvar "nomutt" 0)
  ss
)


;;; ---- COMMANDS ----

;;; COMMAND: Sum numeric values from texts

(defun c:somt (/ ent i ptins ss stlname str_total total value)
  (prompt "\nSOMT - Soma valores num�ricos de textos")
  (ad:inicmd)

  ;; User input
  (setq	ss    (ad:ssgetp
		'((0 . "TEXT") (1 . "#*"))
		"\nSelecione textos que contenham s� n�meros e ponto como separador decimal, e <ENTER> para concluir: "
	      )
	i     0
	total 0.00
  )

  ;; Sum
  (while
    (setq ent (ssname ss i))
     (setq value     (atof (cdr (assoc 1 (entget ent)))) ; NOTE: only leading numberic chars.
	   total     (+ value total)
	   str_total (rtos total 2 2)
	   i	     (1+ i)
     )
  )

  ;; Prompt the user
  (setq str_total (rtos total 2 2))
  (prompt (strcat "\nValor total: " str_total ". "))

  ;; Insert the text
  ;;	TO-DO: configure inset height
  (setvar "osmode" 0)			; turn off OSMODE
  (setq stlname (getvar "textstyle"))
  (if
    (setq ptins (getpoint "Clique para inserir o texto com o valor total: "))
     (if
       (= (cdr (assoc 40 (tblsearch "STYLE" stlname))) 0.0) ; zero as height?
	(command "_text" "_s" stlname "_j" "_mc" ptins 1.0 0 str_total)
	(command "_text" "_s" stlname "_j" "_mc" ptins 0 str_total)
     )
  )

  (ad:endcmd)
  (princ)
)

;;; EOF
