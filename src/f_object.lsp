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

;;; EOF
