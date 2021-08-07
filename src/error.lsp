;;;; error.lsp
;;;; Error handling functions
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2018-01-21
;;;; License: ArcDraft, see LICENSE.txt.


;;;; Create a new error handling for command escaping and errors.


;;; ==== TO-DOs ====
;;; 1.	Create an alist-based system. Some will be mandatory.
;;; 2.  Create an opt-in variable list protection, e.g. (ad:inicmd '([protected-vars-list])).
;;;		Also: (ad:inicmd nil) = none, (ad:inicmd t) = all.


;;; ---- FUNCTIONS ----

;;; FUNCTION: Command ini
;;; 	Saves some values for later and turns off echoing.

(defun ad:inicmd ()

  ;; save values
  (setq *ad:blipmode_or* (getvar "BLIPMODE")  ; blip marks
        *ad:cmdecho_or* (getvar "CMDECHO")    ; echoing
        *ad:dimzin_or* (getvar "DIMZIN")      ; zero suppression
        *ad:osmode_or* (getvar "OSMODE")      ; osmode
        *ad:nomutt_or* (getvar "NOMUTT")      ; command line muttering
        *ad:luprec_or* (getvar "LUPREC")      ; linear units/coordinates precision
        *ad:error_or* *error*        ; error handling
  )

  ;; change values
  (setvar "CMDECHO" 0)
  (setq *error* ad:error)            ; new command error handling

  ;; create undo group
  (command "_undo" "_begin")         ; avoid multiple undoes if the user regrets
  (princ)
)


;;; FUNCTION: Command end
;;; 	If command finishes or on error.

(defun ad:endcmd ()
  (command "_undo" "_end")           ; undo group created

  ;; restore original values
  (setvar "BLIPMODE" *ad:blipmode_or*)
  (setvar "CMDECHO" *ad:cmdecho_or*)
  (setvar "DIMZIN" *ad:dimzin_or*)
  (setvar "OSMODE" *ad:osmode_or*)
  (setvar "NOMUTT" *ad:nomutt_or*)
  (setvar "LUPREC" *ad:luprec_or*)
  (setq *error* *ad:error_or*)
  (princ)
)


;;; FUNCTION: Internal error handling
;;; 	Prompts error message e retores values in case of error.

(defun ad:error (msg)
  (prompt "\nComando cancelado.\n")  ; message
  (ad:endcmd)  ; restore original command settings
)


;;; EOF
