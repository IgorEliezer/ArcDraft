;;;; f_geometric.lsp
;;;; Geometric functions
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2018-01-21
;;;; License: ArcDraft, see LICENSE.txt.


;;;; Functions for points, angles, trigonometry etc (soon).


;;; ---- ANGLES ----

;;; FUNCTION: Get angle from a point of a linear entity

(defun ad:angle_pt (pt / osmode_or pta)
  (setq osmode_or (getvar "OSMODE"))
  (setvar "OSMODE" 0)			; for safety 
  (setq pta (osnap (polar pt 0.00 0.01) "_nea")) ; get a second point along linear entity
  (setvar "OSMODE" osmode_or)

  ;; Get object angle
  (if (not (null pta))			; check if <pta> is valid

    ;; then: get the angle in radians
    (progn
      (if (= 0.0 (distance pt pta))	; check if vertical
	(/ pi 2)
	(angle pt pta)
      )
    )

    ;; else: return nil on error
    nil
  )
)


;;; FUNCTION: Fix angle outside 0-2rad
;;;	Find the lowerest positive coterminal angle.

(defun ad:fixangle (ang)
  (if
    (minusp ang)			; check if negative

     ;; then: add 2 pi until 1st positive angle is found
     (while (minusp ang)
       (setq ang (+ ang (* 2 pi)))
     )

     ;; else: remove 2 pi until lowest positive angle is found
     (while (>= ang (* 2 pi))
       (setq ang (- ang (* 2 pi)))
     )
  )
  ang					; returns
)

;;; EOF
