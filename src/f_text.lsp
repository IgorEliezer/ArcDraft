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


;;; FUNCTION: fix text angle

(defun ad:fixangle_txt (ang)

  (if (minusp ang)			; if negative

    ;; then: add 2 pi until 1st positive angle is found
    (while (minusp ang)
      (setq ang (+ ang (* 2 pi)))
    )

    ;; else: remove 2 pi until lowest positive angle is found
    (while (>= ang (* 2 pi))
      (setq ang (- ang (* 2 pi)))	
    )
  )

  ;; fixing text inversion
  (cond
    ((and (> ang (* 0.5 pi)) (< ang (* 1.0 pi))) ; 90<A<180
     (+ ang pi)				; + 180º
    )
    ((and (>= ang (* 1.0 pi)) (<= ang (* 1.5 pi))) ; 180<=A<=270
     (- ang pi)				; - 180º
    )
    (t ang)
  )
)


;;; FUNCTION: Quick text insert
;;;	<pt> required.
;;;	<h> required. Use nil, t or anything if style has a height (see below).
;;;	<rot>, rotation 0-360, uses the current ANGBASE and ANGDIR. If null, the absolute zero is used.
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
;;; 	Returns true if zoom is enough for text visibility.
;;; 	Issues an alert if zoomed out too much.

(defun ad:textviz ()
  (if
    (not (> (/ (getvar "VIEWSIZE") (* *ad:th* *ad:sc*)) 200.0))
     t
     (alert
       "ArcDraft detectou que o zoom está muito distante para o texto ficar vísivel.

Recomenda-se:
- Aproximar o ZOOM;
- Aumentar a altura de texto ou diminuir a escala pelo comando ACONFIG."
     )
  )
)


;;; EOF
