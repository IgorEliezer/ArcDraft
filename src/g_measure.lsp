;;;; g_measure.lsp
;;;; General commands for measurements, reading and math.
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2018-03-27
;;;; License: ArcDraft, see LICENSE.txt.


;;;; Set up commands for text parsing and math.


;;; ---- FUNCTIONS ----

;;; FUNCTION: SSGET with prompt
;;;	TO-DO: Move it to a special module (f_object.lsp?).
;;; 	NOMUTT is covered by error.lsp

(defun ad:ssgetp (filter msg / ss)
  (prompt msg)
  (setvar "NOMUTT" 1)
  (vl-catch-all-apply '(lambda () (setq ss (ssget filter))))
  (setvar "NOMUTT" 0)
  ss
)


;;; ---- COMMANDS ----

;;; COMMAND: Sum numeric values from texts

(defun c:somt (/ ent height i ptins ss stlname str_total total value)
  (prompt "\nSOMT - Soma valores numéricos de textos")
  (ad:inicmd)

  ;; User input
  (setq	ss    (ad:ssgetp
		'((0 . "TEXT") (1 . "#*,[+]#*,[`-]#*")) ; also accepts + and -
		"\nSelecione textos que começam com números e que usam ponto como separador decimal. <ENTER> para concluir: "
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

  ;; Text height
  ;; 	get it from the last entity
  (setq height (cdr (assoc 40 (entget (ssname ss (1- i)))))) ; go back

  ;; Prompt the user
  (setq str_total (rtos total 2 2))
  (prompt (strcat "\nValor total: " str_total ". "))

  ;; Insert the text
  ;;	TO-DO: let user configure insert height
  (setvar "OSMODE" 0)			; turn off OSMODE
  (setq stlname (getvar "TEXTSTYLE"))
  (if
    (setq ptins (getpoint "Clique para inserir o texto com o valor total: "))
     (if
       (= (cdr (assoc 40 (tblsearch "STYLE" stlname))) 0.0) ; zero as height?
	(command "_text" "_s" stlname "_j" "_mc" ptins height 0 str_total)
	(command "_text" "_s" stlname "_j" "_mc" ptins 0 str_total)
     )
  )

  (ad:endcmd)
  (princ)
)


;;; COMMAND: Get angle from linear entities

(defun c:va (/ ang pt ptins str_ang tip)
  (prompt "\nVA - Obter ângulo de objeto linear")
  (ad:inicmd)

  ;; User input
  (setvar "OSMODE" 512)
  (setq pt (getpoint "\nClique sobre uma entidade linear: "))
  (setq ang (ad:angle_pt pt))  

  ;; Prompt the user
  (if (= 0 (getvar "AUPREC"))
    (setq tip " Dica: Para formato e casas decimais, use _UNITS.") ; tip
    (setq tip "")
  )
  (setq str_ang (strcat (angtos (ad:iang ang)) " ou " (angtos ang)))
  (prompt (strcat "\nÂngulos nos dois sentidos: " str_ang ". " tip))

  ;; Insert text
  ; (getvar "DIMZIN")
  (setvar "OSMODE" 0)
  (if
    (setq ptins (getpoint " Clique para inserir o texto com os ângulos: "))
     (command "_text" "_j" "_mc" ptins 1.0 (angtos ang) (strcat "< " str_ang)) ; hardcoded text height
  )

  (ad:endcmd)
  (princ)
)

;;; EOF
