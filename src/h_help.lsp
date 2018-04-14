;;;; h_help.lsp
;;;; ArcDraft's about and user help.
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2018-01-22
;;;; License: ArcDraft, see LICENSE.txt.


;;;; Help and credits UI.


;;; ---- COMMANDS ----

;;; COMMAND: Ajuda e lista de comandos do ArcDraft

(defun c:aj (/ filename filepath pathfilename)
  (prompt "\nAJ - Ajuda e lista de comandos do ArcDraft")
  (ad:inicmd)

  ;; Alert the user
  (alert
    (strcat
      "*** ArcDraft - Aplicativo para desenho em CAD ***"
      "\nVers�o "
      *ad:ver*
      "\nFeito para AutoCAD 2007 ou superior"
      "\n\nComandos:"
      "\n	AJ - Abrir esta ajuda"
      "\n	AE - Alinhar texto e bloco sem atributo"
      "\n	BB - Quebra no ponto"
      "\n	CUR - Definir camada corrente por sele��o"
      "\n	LBL - Listar blocos de uma sele��o"
      "\n	LLA - Listar camadas de uma sele��o"
      "\n	NLA - Criar camada"
      "\n	NS - Numerador sequencial"
      "\n	SOMT - Soma valores num�ricos de textos"
      "\n	SS - Super save"
      "\n	TT - Transferir textos"
      "\n	UNT - Unir textos"
      "\n	VAA - Obter �rea"
      "\n	VA - Obter �ngulo de objeto linear"
      "\n	VV - Obter �ngulo de v�rtice"
      "\n\nEscrito por Igor Eliezer Borges (igoreliezer.com)"
      "\nDownload e manual: igoreliezer.com/arcdraft"
      "\nVer licen�a em LICENSE.txt."
     )
  )
  (ad:endcmd)
  (princ)
)


;;; FINAL MESSAGES

(prompt	(strcat	"\nArcDraft carregado. Vers�o "
		*ad:ver*
		". (c) Igor Eliezer Borges, ver licen�a em LICENSE.txt."
	)
)
(prompt "\nDigite AJ para abrir a lista de comandos.") ; tip

;;; EOF
