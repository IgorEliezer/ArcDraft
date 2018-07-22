;;;; f_text.lsp
;;;; Text handling functions
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2018-07-21
;;;; License: ArcDraft, see LICENSE.txt.


;;;; Functions for text handling and styles.


;;; ---- FUNCTIONS ----

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
  (ad:textviz)

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
    (> (/ (getvar "VIEWSIZE") (* *ad:th* *ad:sc*)) 200.0)
     (alert
       "ArcDraft detectou que o zoom está muito distante para o texto ficar vísivel.

Recomenda-se:
- Aproximar o ZOOM;
- Aumentar a altura de texto ou diminuir a escala pelo comando ACONFIG."
     )
  )
)

;;; EOF
