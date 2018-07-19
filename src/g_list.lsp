;;;; g_list.lsp
;;;; Commands for listing.
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2018-05-30
;;;; License: ArcDraft, see LICENSE.txt.


;;;; Set up commands for block and layer listing.


;;; ==== TO-DOs ====
;;; 1.	C:GT will go here.


;;; ---- COMMANDS ----

;;; COMMAND: List and count blocks from selection

(defun c:lbl (/ div ent header len luprec_or name result ss total)
  (prompt "\nLBL - Listar blocos de uma sele��o")
  (ad:inicmd)

  ;; User input
  (initget 0 "L C")
  (if
    (= (getkword "\nEscolha uma op��o [Listar/Contar de um bloco] <Listar>: ") "C")

     ;; - Count
     (progn
       (if
	 (= (setq name (getstring t "\nDigite o nome do bloco ou <obter>: ")) "")
	  (progn
	    (while
	      (/= "INSERT"
		  (cdr (assoc 0 (entget (car (setq ent (entsel "\nSelecione um bloco: "))))))
	      )
	       (prompt "\nEste objeto n�o � um bloco!")
	    )
	    (setq name (cdr (assoc 2 (entget (car ent)))))
	  )
       )
       (setq ss	(ad:ssgetp
		  (list '(0 . "INSERT") (cons 2 name))
		  (strcat " Nome do bloco: \"" name "\".\nSelecione objetos para contar: ")
		)
       )
       (if ss
	 (setq len (sslength ss))
	 (setq len 0)
       )
       (alert (strcat "\nH� " (itoa len) " blocos com o nome \"" name "\" na sele��o.")
       )
     )

     ;; - List
     (progn
       (setq result
	      (ad:block-counter
		(ad:ssgetp '((0 . "INSERT")) "\nSelecione objetos para listar blocos: ")
	      )
       )

       ;; Propmt the user
       (if result
	 (progn
	   (setq total	(strcat "N� de nomes de blocos: " (itoa (length result)))
		 header	"\nNome do bloco (Quantidade)"
		 div	"\n-------------------------------"
	   )

	   ;; Pop-up
	   (setq luprec_or (getvar "LUPREC"))
	   (setvar "LUPREC" 0)
	   (alert (strcat total div header div "\n" (ad:alist->str result " (" ")\n") ")"))
	   (setvar "LUPREC" luprec_or)

	   ;; Command line
	   (prompt "\n\n")
	   (prompt total)
	   (prompt div)
	   (prompt header)
	   (prompt div)
	   (foreach line result
	     (prompt (strcat "\n" (car line) " (" (itoa (cdr line)) ")"))
	   )
	   (prompt div)
	   (prompt "\nPressione F2 para ver resultados.")
	 )
	 (prompt "\nNenhum bloco foi selecionado.")
       )
     )
  )
  (ad:endcmd)
  (princ)
)


;;; COMMAND: List layers and count entities from selection

(defun c:lla (/ div header len luprec_or name result ss total)
  (prompt "\nLLA - Listar camadas de uma sele��o")
  (ad:inicmd)

  ;; User input
  (initget 0 "L C")
  (if
    (= (getkword "\nEscolha uma op��o [Listar/Contar de uma camada] <Listar>: ") "C")

     ;; - Count
     (progn
       (if
	 (= (setq name (getstring t "\nDigite o nome da camada ou <obter>: ")) "")
	  (setq name (cdr (assoc 8 (entget (car (entsel "\nSelecione um objeto: "))))))
       )
       (setq ss	(ad:ssgetp
		  (list (cons 8 name))
		  (strcat " Nome da camada: \"" name "\".\nSelecione objetos para contar: ")
		)
       )
       (if ss
	 (setq len (sslength ss))
	 (setq len 0)
       )
       (alert (strcat "\nH� " (itoa len) " objetos na camada \"" name "\" na sele��o.")
       )
     )

     ;; - List
     (progn
       (setq result (ad:layer-counter
		      (ad:ssgetp nil "\nSelecione objetos para listar camadas: ")
		    )
       )

       ;; Propmt the user
       (if result
	 (progn
	   (setq total	(strcat "N� de camadas: " (itoa (length result)))
		 header	"\nNome da camada (Quantidade)"
		 div	"\n-------------------------------"
	   )

	   ;; Pop-up
	   (setq luprec_or (getvar "LUPREC"))
	   (setvar "LUPREC" 0)
	   (alert (strcat total div header div "\n" (ad:alist->str result " (" ")\n") ")"))
	   (setvar "LUPREC" luprec_or)

	   ;; Command line
	   (prompt "\n\n")
	   (prompt total)
	   (prompt div)
	   (prompt header)
	   (prompt div)
	   (foreach line result
	     (prompt (strcat "\n" (car line) " (" (itoa (cdr line)) ")"))
	   )
	   (prompt div)
	   (prompt "\nPressione F2 para ver resultados.")
	 )
	 (prompt "\nNenhum objeto foi selecionado.")
       )
     )
  )

  (ad:endcmd)
  (princ)
)

;;; EOF
