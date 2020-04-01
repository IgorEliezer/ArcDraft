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
      "\n"
      "\nATALHO\t COMANDO"
      "\n ?A \t - Abrir esta ajuda"
      "\n ACONFIG\t - Configuração básica do ArcDraft"
      "\n AE \t - Alinhar texto e bloco sem atributo"
      "\n AN \t - Achar nível a partir de duas cotas de nível"
      "\n BB \t - Quebra no ponto"
      "\n CUR \t - Definir camada corrente por seleção"
      "\n GTC \t - Gera tabela de coordenadas de textos de uma camada"
      "\n EVP \t - Escala de Viewport"
      "\n ICO \t - Inserir coordenadas X e Y" 
      "\n II \t - Inserir inclinação"
      "\n LBL \t - Listar blocos de uma seleção"
      "\n LLA \t - Listar camadas de uma seleção"
      "\n NLA \t - Criar camada"
      "\n NOTA \t - Nota com chamada"
      "\n NS \t - Numerador sequencial"
      "\n OSP \t - OSNAP pré-definido/recomendado"
      "\n SCM \t - Scale múltiplo"
      "\n SOMPL \t - Somar comprimentos de polilinhas abertas"
      "\n SOMT \t - Somar valores numéricos de textos"
      "\n SS \t - Super save"
      "\n TT \t - Transferir textos"
      "\n UNT \t - Unir textos"
      "\n VAA \t - Ver área"
      "\n VA \t - Ver ângulo de objeto linear"
      "\n VV \t - Ver ângulo de vértice"
      "\n VPL \t - Ver comprimento de polilinha aberta"
      "\n\nEscrito por Igor Eliezer Borges (igoreliezer.com)"
      "\nPágina oficial, download e manual: igoreliezer.com/arcdraft"
      "\nVer licença em LICENSE.txt."
     )
  )
  (ad:endcmd)
  (princ)
)


;;; FINAL MESSAGES

(prompt
  (strcat "\nArcDraft carregado. Versão "
	  *ad:ver*
	  ". (c) Igor Eliezer Borges Arquiteto e Urbanista, ver licença em LICENSE.txt."
  )
)
(prompt "\nDigite ?A para lista de comandos, e ACONFIG para configurações.") ; tip


;;; EOF

