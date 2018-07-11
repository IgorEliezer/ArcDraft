;;;; arcdraft.lsp
;;;; ArcDraft startup!
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2018-01-21
;;;; License: ArcDraft, see LICENSE.txt.


;;;; First set ArcDraft version and check support.


;;;; /!\ ArcDraft REQUIRES AutoCAD 2007 OR HIGHER /!\
;;;; Custom functions and global variables are prefixed with "ad:"
;;;; FOR FUTURE: BricsCAD support (it already does, but not officially)


;;; ---- HELLO WORLD! ----

(prompt "\n\nCarregando ArcDraft... ")	; prompt loading


;;; FUNCTION: Set ArcDraft version

(defun ad:version ()
  (setq *ad:ver* "0.1 beta, 2018-07-??") ; version number, stage and date
)

(ad:version)				; execute


;;; ---- COMPABILITY ----

;;; FUNCTION: Get CAD version and check if it supports ArcDraft
;;;	AutoCAD version 17 or higher.

(defun ad:cadsupport (/ acadvernum)
  (prompt "\nVerificando compatibilidade com o AutoCAD... ")
  (setq acadvernum (substr (getvar "ACADVER") 1 2))
  (if
    (< (atoi acadvernum) 17)		; if earlier than AutoCAD 2007, v17

     ;; then: alert the user and exit
     (progn
       (alert
	 "A versão de AutoCAD que você está usando não é compatível com ArcDraft!\n
O ArcDraft requer AutoCAD 2007 ou superior para funcionar."
       )
       (prompt
	 "Erro: Versão do AutoCAD incompatível com ArcDraft! Carregamento cancelado."
       )
       (exit)
     )

     ;; else: continue
     (progn
       (prompt "OK.")
       t
     )
  )
)

(ad:cadsupport)				; execute


;;; FUNCTION: Check if drawing is in meters and warn user if not.

(defun ad:check-dwg (/ key)
  (if
    (/= (getvar "insunits") 6)
     (progn
       (initget 0 "Sim Não")
       (setq key
	      (getkword
		"\nO desenho não está em metros. Deseja corrigir isso? (pode-se mudar depois pelo comando _UNITS) [Sim/Não] <Sim>: "
	      )
       )
       (if
	 (member key '(nil "Sim"))
	  (progn
	    (setvar "INSUNITS" 6)
	    (prompt "\n:: Unidade: metros.")
	  )
	  (prompt "\nA unidade não foi alterada!")
       )
     )
    (prompt "\n:: Unidade: metros.")
  )
)

(ad:check-dwg)				; execute


;; it's all set! Now go ahead to load ArcDraft...

;;; EOF
