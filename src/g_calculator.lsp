;;;; g_calculator.lsp
;;;; General commands for measurement calculation.
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2018-08-04
;;;; License: ArcDraft, see LICENSE.txt.


;;;; Set up commands for measurement calculation.


;;; ---- COMMANDS ----

;;; COMMAND: Find elevation

(defun c:an (/ ang dh dist h1 h2 hi hmax hmin pta ptins pt_hi pt_hmax pt_hmin str_ele)
  (prompt "AN - Achar nível entre dois pontos conhecidos")
  (ad:inicmd)

  (setvar "DIMZIN" 0)

  ;; User input - 1st height
  (initget 0)
  (if (null (setq h1 (getreal "\nDigite o valor de um nível ou <obter>: ")))
    (setq h1
	   (atof
	     (cdr (assoc 1 (cdr (entget (car (nentsel "\nSelecione um texto numérico: "))))))
	   )
    )
  )
  (prompt (strcat "\nAltura obtida: " (rtos h1) "."))

  ;; User input - 2nd height
  (initget 0)
  (if (null (setq h2 (getreal "\nDigite o valor de outro nível ou <obter>: ")))
    (setq h2
	   (atof
	     (cdr (assoc 1 (cdr (entget (car (nentsel "\nSelecione um texto numérico: "))))))
	   )
    )
  )
  (prompt (strcat "\nAltura obtida: " (rtos h2) "."))

  ;; Find the highest and lowest values, get the points and calculate
  (setvar "BLIPMODE" 1)
  (setvar "OSMODE" 521)			; end, nod, nea
  (setq	hmax	(max h1 h2)		; highest value
	hmin	(min h1 h2)		; lowest value
	dh	(- hmax hmin)		; find d-height
	pt_hmin	(getpoint "\nClique no ponto correspondente ao nível mais baixo: ")
	pt_hmax	(getpoint pt_hmin "\nClique no ponto correspondente ao nível mais alto: ")
	ang	(angle pt_hmin pt_hmax)
  )

  ;; Intermediary points
  (while
    (progn
      (initget 0 "L")
      (setq pta
	     (getpoint
	       pt_hmin
	       "\nEspecifique o ponto onde se deseja obter o nível intermediário ou [Localizar nível]: "
	     )
      )
    )
     (if (= pta "L")			; option 'L'

       ;; then: find the position of the height
       (progn
	 (setq hi    (getreal "\nDigite o valor do nível a localizar no intervalo: ")
	       dist  (* (/ (- hi hmin) dh) (distance pt_hmin pt_hmax))
	       pt_hi (polar pt_hmin ang dist)
	 )
	 (command "_point" pt_hi)	; place a point
       )

       ;; else: find the heights of the given points
       (progn
	 (setq pt_hmax (list (car pt_hmax) (cadr pt_hmax) 0.0) ; z0
	       pt_hmin (list (car pt_hmin) (cadr pt_hmin) 0.0) ; z0
	       pta     (list (car pta) (cadr pta) 0.0) ; z0
	       pt_hi   (inters pt_hmin pt_hmax pta (polar pta (+ ang (/ pi 2)) 1) nil)
	 )
	 (if
	   (member (ad:quadrant ang (angle pt_hmin pt_hi)) '(1 4)) ; same direction
	    (setq hi (+ hmin (* dh (/ (distance pt_hmin pt_hi) (distance pt_hmin pt_hmax)))))
	    (setq hi (- hmin (* dh (/ (distance pt_hmin pt_hi) (distance pt_hmin pt_hmax)))))
	 )

	 ;; Prompt the user
	 (setq str_ele (rtos hi))
	 (prompt (strcat "\nNível intermediário: " str_ele ". "))

	 ;; Insert the text
	 (setvar "OSMODE" 0)
	 (setvar "BLIPMODE" 0)
	 (if
	   (setq ptins (getpoint "Clique para inserir o texto com o valor ou <sair>: "))
	    (ad:text str_ele "_bc" ptins (* *ad:th* *ad:sc*) nil)
	 )
       )
     )
     (setvar "BLIPMODE" 1)
  )
  (setvar "BLIPMODE" 0) ; TO-DO: review code for a better way to toggle BLIPMODE.
  
  (ad:endcmd)
  (princ)
)


;;; COMMAND: Calculate incline

(defun c:ii (/ dh dist h1 h2 incl pt1 pt2 ptins str_incl)
  (prompt "II - Calcular inclinação")
  (ad:inicmd)

  (setvar "DIMZIN" 0)

  ;; Get user input - 1st height
  (initget 0)
  (if (null (setq h1 (getreal "\nDigite o valor do nível inicial ou <obter>: ")))
    (progn
      (setq
	h1 (atof
	     (cdr (assoc 1 (cdr (entget (car (nentsel "\nSelecione um texto numérico: "))))))
	   )
      )
      (prompt (strcat "\nDado obtido: " (rtos h1) "."))
    )
  )

  ;; Get user input - 2nd height
  (initget 0)
  (if (null (setq h2 (getreal "\nDigite o valor do nível final ou <obter>: ")))
    (progn
      (setq
	h2 (atof
	     (cdr (assoc 1 (cdr (entget (car (nentsel "\nSelecione um texto numérico: "))))))
	   )
      )
      (prompt (strcat "\nDado obtido: " (rtos h2) "."))
    )
  )

  ;; Get user input - distance
  ;; 	TO-DO: 'Calculate' option to find the distance from dh / incl = dist
  (initget 0)
  (if (null (setq dist (getreal "\nDigite a distância ou <obter de 2 pontos>: ")))
    (progn
      (setq pt1	 (getpoint "\nEspecifique o primeiro ponto: ")
	    pt2	 (getpoint pt1 "\nEspecifique o segundo ponto: ")
	    dist (distance pt1 pt2)
      )
      (prompt (strcat "\nDado obtido: " (rtos dist) "."))
    )
  )

  ;; Calculate <incl>
  (if
    (and h1 h2 dist)

     ;; then: calculate
     (progn
       (setq dh	  (- h2 h1)
	     incl (/ dh dist)
       )
       (prompt
	 (strcat "\nInclinação: fator " (rtos incl) " (" (rtos (* incl 100)) "%). ")
       )

       ;; Insert the text
       (setvar "OSMODE" 0)
       (if
	 (setq ptins (getpoint "Clique para inserir o texto com o valor: "))
	  (progn
	    (setq str_incl (strcat (rtos (* incl 100)) "%"))
	    (ad:text str_incl "_bc" ptins (* *ad:th* *ad:sc*) nil)
	  )
       )
     )

     ;; else: prompt the use and exit
     (prompt "\nValor inválido!")
  )

  (ad:endcmd)
  (princ)
)

;;; EOF
