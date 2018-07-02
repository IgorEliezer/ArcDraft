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
      "\nVersão "
      *ad:ver*
      "\nFeito para AutoCAD 2007 ou superior"
      "\n\nComandos:"
      "\n	?A - Abrir esta ajuda"
      "\n	AE - Alinhar texto e bloco sem atributo"
      "\n	BB - Quebra no ponto"
      "\n	CUR - Definir camada corrente por seleção"
      "\n	EST - Escala de trabalho"
      "\n	EVP - Escala de Viewport"
      "\n	ICO - Inserir coordenadas X e Y"
      "\n	LBL - Listar blocos de uma seleção"
      "\n	LLA - Listar camadas de uma seleção"
      "\n	NLA - Criar camada"
      "\n	NS - Numerador sequencial"
      "\n	OSP - OSNAP pré-definido/recomendado"
      "\n	SCM - Scale múltiplo"
      "\n	SOMPL - Somar comprimentos de polilinhas abertas"
      "\n	SOMT - Somar valores numéricos de textos"
      "\n	SS - Super save"
      "\n	TT - Transferir textos"
      "\n	UNT - Unir textos"
      "\n	VAA - Obter área"
      "\n	VA - Obter ângulo de objeto linear"
      "\n	VV - Obter ângulo de vértice"
      "\n	VPL - Obter comprimento de polilinha aberta"
      "\n\nEscrito por Igor Eliezer Borges (igoreliezer.com)"
      "\nPágina oficial, download e manual: igoreliezer.com/arcdraft"
      "\nVer licença em LICENSE.txt."
     )
  )
  (ad:endcmd)
  (princ)
)


;;; FINAL MESSAGES

(prompt	(strcat	"\nArcDraft carregado. Versão "
		*ad:ver*
		". (c) Igor Eliezer Borges Arquiteto e Urbanista, ver licença em LICENSE.txt."
	)
)
(prompt "\nDigite ?A para abrir a lista de comandos.") ; tip

;;; EOF
