;;;; h_help.lsp
;;;; ArcDraft's about and user help.
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2018-01-22
;;;; License: ArcDraft, see LICENSE.txt.


;;;; Help and credits UI.


;;; ---- COMMANDS ----

;;; COMMAND: Ajuda e lista de comandos do ArcDraft

(defun c:?a (/ filename filepath pathfilename)
  (prompt "\n?A - Ajuda e lista de comandos do ArcDraft")
  (ad:inicmd)

  ;; Alert the user
  (alert
    (strcat
      "*** ArcDraft - Aplicativo para desenho em CAD ***"
      "\nVers�o "
      *ad:ver*
      "\nFeito para AutoCAD 2007 ou superior"
      "\n\nComandos:"
      "\n	?A - Abrir esta ajuda"
      "\n	AE - Alinhar texto e bloco sem atributo"
      "\n	BB - Quebra no ponto"
      "\n	CUR - Definir camada corrente por sele��o"
      "\n	EST - Escala de trabalho"
      "\n	EVP - Escala de Viewport"
      "\n	ICO - Inserir coordenadas X e Y"
      "\n	LBL - Listar blocos de uma sele��o"
      "\n	LLA - Listar camadas de uma sele��o"
      "\n	NLA - Criar camada"
      "\n	NS - Numerador sequencial"
      "\n	OSP - OSNAP pr�-definido/recomendado"
      "\n	SCM - Scale m�ltiplo"
      "\n	SOMPL - Somar comprimentos de polilinhas abertas"
      "\n	SOMT - Somar valores num�ricos de textos"
      "\n	SS - Super save"
      "\n	TT - Transferir textos"
      "\n	UNT - Unir textos"
      "\n	VAA - Obter �rea"
      "\n	VA - Obter �ngulo de objeto linear"
      "\n	VV - Obter �ngulo de v�rtice"
      "\n	VPL - Obter comprimento de polilinha aberta"
      "\n\nEscrito por Igor Eliezer Borges (igoreliezer.com)"
      "\nP�gina oficial, download e manual: igoreliezer.com/arcdraft"
      "\nVer licen�a em LICENSE.txt."
     )
  )
  (ad:endcmd)
  (princ)
)


;;; FINAL MESSAGES

(prompt	(strcat	"\nArcDraft carregado. Vers�o "
		*ad:ver*
		". (c) Igor Eliezer Borges Arquiteto e Urbanista, ver licen�a em LICENSE.txt."
	)
)
(prompt "\nDigite ?A para abrir a lista de comandos.") ; tip

;;; EOF
