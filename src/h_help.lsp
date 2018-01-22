;;;; h_help.lsp
;;;; ArcDraft's about and user help.
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2018-01-22
;;;; License: OpenSource - The MIT License (MIT)


;;;; Help UI.


;;; ---- COMMANDS ----

;;; AJ - Ajuda e lista de comandos do ArcDraft

(defun c:aj (/ filename filepath pathfilename)
  (prompt "\nAJ - Ajuda e lista de comandos do ArcDraft")
  (ad:inicmd)

  ;; Alert the user
  (alert
    (strcat "*** ArcDraft - Aplicativo para desenho em CAD ***"
	    "\nVers�o "
	    *ad:ver*
	    "\n\nComandos:"
	    "\n	AE - Alinha texto e bloco sem atributo"
	    "\n	CUR - Definir camada corrente por sele��o"
	    "\n	NLA - Criar camada"
	    "\n	NS - Numerador sequencial"
	    "\n	TT - Transfere textos"
	    "\n	UNT - Unir textos"
	    "\n\nFeito para AutoCAD 2007 ou superior."
	    "\n\nEscrito por Igor Eliezer Borges (igoreliezer.com)\nLicen�a OpenSource/MIT."
	    "\n\n[Rodando AutoCAD r"
	    (substr (getvar "ACADVER") 1 4)
	    ", processador "
	    (getenv "PROCESSOR_ARCHITECTURE")
	    "]"
    )
  )

  (ad:endcmd)
  (princ)
)


;;; FINAL MESSAGES

(prompt	(strcat	"\nArcDraft carregado. Vers�o "
		*ad:ver*
		". (c) Igor Eliezer Borges, licen�a OpenSource/MIT."
	)
)
(prompt "\nDigite AJ para abrir a lista de comandos.") ; tip

;;; EOF