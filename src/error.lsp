;;;; error.lsp
;;;; Error handling functions
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2018-01-21
;;;; License: ArcDraft, see LICENSE.txt.


;;;; Create a new error handling for command escaping and errors.


;;; ==== TO-DOs ====
;;; 1.	Protect LUPREC.
;;; 2.	Create an alist-based system.
;;; 3.  Create an opt-in variable list protection, e.g. (ad:inicmd '([protected-vars-list])).
;;;		Also: (ad:inicmd nil) = none, (ad:inicmd t) = all.


;;; ---- FUNCIONS ----

;;; FUNCTION: Command ini
;;; 	Saves some values for later and turns off echoing.

(defun ad:inicmd ()

  ;; save values
  (setq	*ad:cmdecho_or*	(getvar "CMDECHO") ; echoing
	*ad:dimzin_or*	(getvar "DIMZIN") ; zero suppression
	*ad:osmode_or*	(getvar "OSMODE") ; osmode
	*ad:nomutt_or*	(getvar "NOMUTT") ; command line muttering
	*ad:error_or*	*error*		; error handling
  )

  ;; change values
  (setvar "CMDECHO" 0)
  (setq *error* ad:error)		; new command error handling

  ;; create undo group
  (command "_undo" "_begin")		; avoid multiple undoes if the user regrets
  (princ)
)


;;; FUNCTION: Command end
;;; 	If command finishes or on error.

(defun ad:endcmd ()
  (command "_undo" "_end")		; undo group created

  ;; restore original values
  (setvar "CMDECHO" *ad:cmdecho_or*)
  (setvar "DIMZIN" *ad:dimzin_or*)  
  (setvar "OSMODE" *ad:osmode_or*)
  (setvar "NOMUTT" *ad:nomutt_or*)
  (setq *error* *ad:error_or*)
  (princ)
)


;;; FUNCTION: Internal error handling
;;; 	Prompts error message e retores values in case of error.

(defun ad:error	(msg)
  (prompt "\nComando cancelado.\n")	; message
  (ad:endcmd)				; restore original command settings
)

;;; EOF
