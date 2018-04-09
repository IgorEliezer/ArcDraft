;;;; f_list.lsp
;;;; List functions
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2018-04-06
;;;; License: ArcDraft, see LICENSE.txt.


;;;; Functions for list handling.


;;; ---- LISTS ----

;;; FUNCTION: list to string

(defun ad:lst->str (lst sep / item str)

  (while lst
    (if	(numberp (setq item (car lst)))
      (setq item (rtos item))		; covert to STR
    )

    ;; Build
    (if	str
      (setq str (strcat str sep item))	; add
      (setq str item)			; create
    )
    (setq lst (cdr lst))
  )
  str
)


;;; ---- ASSOC LISTS ----

;;; FUNCTION: String assoc list to string

(defun ad:alist->str (alist sep_pair sep / key pair str val)

  (while alist
    (setq pair (car alist))
    (if	(numberp (setq key (car pair)))
      (setq key (rtos key))		; covert to STR
    )
    (if	(numberp (setq val (cdr pair)))
      (setq val (rtos val))		; covert to STR
    )
    (setq pair (strcat key sep_pair val)) ; combine

    ;; Build
    (if	str
      (setq str (strcat str sep pair))	; add
      (setq str pair)			; create
    )
    (setq alist (cdr alist))
  )
  str
)

;;; EOF
