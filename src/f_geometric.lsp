;;;; f_geometric.lsp
;;;; Geometric functions
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2018-01-21
;;;; License: ArcDraft, see LICENSE.txt.


;;;; Functions for points, angles, trigonometry etc (soon).


;;; ---- POINTS ----

;;; FUNCTION: Get the mid point between two 2D points

(defun ad:ptmed	(pt1 pt2 / k1 k2)
  (mapcar '(lambda (k1 k2) (/ (+ k1 k2) 2.0)) pt1 pt2)
)


;;; ---- LENGTH ----

;;; FUNCTION: Get length of LWPOLYLINE
;;;	WARNING! This function doesn't measure the last seg in closed LWPL.
;;;	TO-DO: Implement closed LWPL detection.

(defun ad:pllen	(ent / bulge delta entlist i key len len_total pair pta ptb r)

  ;; Get data
  (setq entlist (entget ent))
  (if (= (cdr (assoc 0 entlist)) "LWPOLYLINE")
    (progn
      (setq len_total 0.0
	    i 1
      )
      (while (setq pair (nth i entlist))
	(setq key (car pair))
	(cond
	  ((and (= key 10) (null pta))
	   (setq pta (cdr pair))
	  )
	  ((= key 42)
	   (setq bulge (cdr pair))
	  )
	  ((and (= key 10) pta)
	   (setq ptb (cdr pair))
	  )
	)

	;; Calculate
	(if (and pta bulge ptb)
	  (progn
	    (if
	      (= bulge 0.0)
	       (setq len (distance pta ptb)) ; straight line
	       (setq delta (abs (* 4.0 (atan bulge))) ; included angle
		     r	   (/ (distance pta ptb) (* 2.0 (sin (/ delta 2.0)))) ; radius
		     len   (* delta r)
	       )
	    )
	    (setq pta	    ptb		; move pta->ptb
		  ptb	    nil		; for next vertex
		  len_total (+ len_total len) ; sum
	    )
	  )
	)
	(setq i (1+ i))
      )

      len_total				; return
    )
    nil
  )
)


;;; ---- ANGLES ----

;;; FUNCTION: Get angle from a point of a linear entity
;;;	It's not affected by ANGBASE and ANGDIR.
;;; 	Example: (ad:angle_pt (getpoint "\nPoint: "))

(defun ad:angle_pt (pt / osmode_or pta)
  (setq osmode_or (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  (setq pta (osnap (polar pt 0.00 0.01) "_nea")) ; get a second point along linear entity
  (setvar "OSMODE" osmode_or)

  ;; Get object angle
  (if pta				; check if <pta> is valid

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


;;; FUNCTION: Invert angle
;;; 	Add or subtract 1 pi from angles.

(defun ad:iang (ang)
  (if
    (and (>= ang 0) (< ang pi))		; assumes always 0..2 pi rad
     (+ ang pi)
     (- ang pi)
  )
)


;;; FUNCTION: Find the quadrant of an angle, 
;;;	Considers the base angle
;;;	Returns the quadrant number (I, II, III and IV)

(defun ad:quadrant (ang_ref ang_check / ang)
  (setq ang (ad:fixangle (- ang_check ang_ref)))
  
  (cond
    ((and (>= ang 0) (<= ang (* 0.5 pi)))
     1
    )
    ((and (> ang (* 0.5 pi)) (<= ang (* 1.0 pi)))
     2
    )
    ((and (> ang (* 1.0 pi)) (<= ang (* 1.5 pi)))
     3
    )
    ((or (> ang (* 1.5 pi)) (< ang 0))
     4
    )
  )
)

;;; EOF
