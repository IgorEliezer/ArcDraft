;;;; f_object.lsp
;;;; Object handling functions
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2018-04-04
;;;; License: ArcDraft, see LICENSE.txt.


;;;; Functions for entity handling and listing.


;;; ---- SELECTION ----

;;; FUNCTION: SSGET with filter and prompt
;;; 	NOMUTT is covered by error.lsp

(defun ad:ssgetp (filter msg / ss)
  (prompt msg)
  (setvar "NOMUTT" 1)
  (vl-catch-all-apply '(lambda () (setq ss (ssget filter))))
  (setvar "NOMUTT" 0)			; important!
  ss
)


;;; ---- BLOCK ----

;;; FUNCTION: List blocks by name from selection
;;; 	Example: (ad:block-list (ssget))

(defun ad:block-list (ss / blkname entdata i lst_blk)
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
	(setq entdata (entget (ssname ss i)))
	(if (= (cdr (assoc 0 entdata)) "INSERT")
	  (progn
	    (setq blkname (cdr (assoc 2 entdata)))
	    (if
	      (not (member blkname lst_blk))
	       (setq lst_blk (append lst_blk (list blkname)))
	    )
	  )
	)
	(setq i (1+ i))
      )
      lst_blk
    )
    nil
  )
)


;;; FUNCTION: List and count blocks by name from selection
;;; 	Example: (ad:block-counter (ssget))

(defun ad:block-counter	(ss / alst blkname entdata i pair pair_new)
  (if ss
    (progn
      (setq i	 0
	    alst nil
      )
      (repeat (sslength ss)
	(setq entdata (entget (ssname ss i)))
	(if (= (cdr (assoc 0 entdata)) "INSERT")

	  ;; Get block name and update list
	  (progn
	    (setq blkname (cdr (assoc 2 entdata)))
	    (if	(setq pair (assoc blkname alst)) ; pair exists in the list

	      ;; 1+ the count in the pair
	      (setq pair_new (cons blkname (1+ (cdr pair)))
		    alst     (subst pair_new pair alst) ; update list
	      )
	      ;; Create pair and add to list
	      (setq alst (append alst (list (cons blkname 1))))
	    )
	  )
	)
	(setq i (1+ i))
      )
      alst
    )
    nil
  )
)


;;; ---- LAYER ----

;;; FUNCTION: Make layer

(defun ad:mlayer (name color ltype)
  (if
    (tblsearch "LAYER" name)
     (setvar "CLAYER" name)
     (command "_layer" "_make" name "_color" color name "_ltype" ltype name "")
  )
)


;;; ---- TEXT ----

;;; FUNCTION: Make style
;;; 	Example: (ad:mstyle "Test" "Tahoma" 1.0)

(defun ad:mstyle (name font h)
  (if
    (tblsearch "STYLE" name)
     (setvar "TEXTSTYLE" name)
     (command "_style" name font h "1.00" "0" "N" "N")
  )
)


;;; FUNCTION: Quick text insert
;;;	<pt> required.
;;;	<h> required. Use nil, t or anything if style has a height (see below).
;;;	<rot>, rotation, uses the current ANGBASE and ANGDIR. If null, the absolute zero is used.
;;;	Example: (ad:text "String" "_c" (getpoint "\nPonto: ") 5.5 15)

(defun ad:text (content justify pt h rot / angbase style)

  ;; Justify
  (if (null justify)
    (setq justify "_mc")
  )

  ;; Rotation
  (if (null rot)
    (progn
      (setq angbase (* (/ (getvar "ANGBASE") pi) 180.0))
      (if (= (getvar "ANGDIR") 0)
	(setq rot (- angbase))
	(setq rot angbase)
      )
    )
  )

  ;; <content> is required - insert text
  (if					; if style has a height, drop <h>
    (= (cdr (assoc 40 (tblsearch "STYLE" (getvar "TEXTSTYLE")))) 0.0)
     (command "_text" "_j" justify pt h rot content)
     (command "_text" "_j" justify pt rot content)
  )
)


;;; FUNCTION: Check text visibility
;;; 	It does nothing else than warning the user, for now.

(defun ad:textviz ()
  (if
    (> (/ (getvar "VIEWSIZE") *ad:th*) 20.0)
     (alert
       "Aviso: Seu zoom está muito distante para o texto ficar vísivel.

Você pode:
- Aproximar o ZOOM;
- Aumentar a altura de texto pelo comando ACONFIG."
     )
  )
)

;;; EOF
