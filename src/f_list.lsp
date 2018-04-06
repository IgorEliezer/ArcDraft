;;;; f_list.lsp
;;;; List functions
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2018-04-06
;;;; License: ArcDraft, see LICENSE.txt.


;;;; Functions for list handling Starting with assoc list to list.


;;; ---- ASSOC LISTS ----

;;; FUNCTION: String assoc list to single string
;;; 	TO-DO: make it support numbers.

(defun ad:alist->str (alist sep_pair sep / i lst-str str)
  (setq lst-str (mapcar '(lambda (pair) (strcat (car pair) sep_pair (cdr pair))) alist))
  (setq	str (car lst-str)
	i   1
  )
  (while (setq pair (nth i lst-str))
    (setq str (strcat str sep pair)
	  i   (1+ i)
    )
  )
  str
)

;;; EOF
