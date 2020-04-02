;;;; arcdraft.lsp
;;;; ArcDraft startup!
;;;; Written by Igor Eliezer Borges, Architect and Urban Planner
;;;; http://www.igoreliezer.com
;;;; Date: 2018-01-21
;;;; License: ArcDraft, see LICENSE.txt.


;;;; First set ArcDraft version and check support.


;;;; /!\ ArcDraft REQUIRES AutoCAD 2007 or BricsCAD 17 OR HIGHER /!\
;;;; Custom functions and global variables are prefixed with "ad:"


;;; ---- HELLO WORLD! ----

(prompt "\n\nCarregando ArcDraft... ")    ; prompt loading


;;; FUNCTION: Set ArcDraft version

(defun ad:version ()
  (setq *ad:ver* "0.3 beta, 2020-04-??")  ; version number, stage and date
)

(ad:version)  ; execute


;;; ---- COMPABILITY ----

;;; FUNCTION: Get CAD version and check if it supports ArcDraft

(defun ad:cadsupport (/ acadvernum)
  (prompt "\nVerificando compatibilidade com o CAD... ")
  (setq acadvernum (substr (getvar "ACADVER") 1 2))
  (if
    (< (atoi acadvernum) 17)  ; if earlier than release 17

    ;; then: alert the user and exit
    (progn
      (alert
        "A versão de AutoCAD que você está usando não é compatível com ArcDraft!\n
O ArcDraft requer AutoCAD 2007, BricsCAD 17 ou superior para funcionar."
      )
      (prompt
        "Erro: Versão de CAD incompatível com ArcDraft! Carregamento cancelado."
      )
      (exit)
    )

    ;; else: continue
    (progn
      (prompt "OK.")
      t
    )
  )
)

(ad:cadsupport)  ; execute


;;; FUNCTION: Working ArcDraft path
;;;   Todo: implement special alerts for AutoCAD and BricsCAD.

(defun ad:path (/ file filepath)
  (setq file "arcdraft.lsp")
  (if
    (setq filepath (findfile file))
    (setq *ad:path* (substr filepath 1 (- (strlen filepath) 13)))
    (progn
      (prompt (strcat "\nErro: A pasta do " file " não foi encontrada. Carregamento cancelado!"))
      (setq *ad:path* nil)
      (exit)
    )
  )
)

(ad:path)        ; execute


;;; FUNCTION: Load files

(defun ad:load_libs (/ ad_libs lib)
  (if *ad:path*
    (progn
      (setq ad_libs (list
                      (strcat *ad:path* "\\error.lsp")
                      (strcat *ad:path* "\\f_command.lsp")
                      (strcat *ad:path* "\\f_geometric.lsp")
                      (strcat *ad:path* "\\f_list.lsp")
                      (strcat *ad:path* "\\f_object.lsp")
                      (strcat *ad:path* "\\f_text.lsp")
                      (strcat *ad:path* "\\g_aconfig.lsp")
                      (strcat *ad:path* "\\g_calculator.lsp")
                      (strcat *ad:path* "\\g_edit.lsp")
                      (strcat *ad:path* "\\g_file.lsp")
                      (strcat *ad:path* "\\g_file.lsp")
                      (strcat *ad:path* "\\g_layer.lsp")
                      (strcat *ad:path* "\\g_list.lsp")
                      (strcat *ad:path* "\\g_measure.lsp")
                      (strcat *ad:path* "\\g_text.lsp")
                      (strcat *ad:path* "\\h_help.lsp")
                    )
      )
      (if
        (apply 'and (mapcar 'findfile ad_libs))
        (progn
          (prompt "\nLibs encontradas. Carregando: ")
          (foreach lib ad_libs (load lib))
        )
        (progn
          (prompt
            "\nErro: Nem todas as libs foram encontradas! Carregamento cancelado."
          )
          (setq ad_libs nil)
          (exit)
        )
      )
    )
  )
  ad_libs
)

(ad:load_libs)   ; execute


;;; FUNCTION: Check if drawing is in meters and warn user if not.

(defun ad:check-dwg (/ key)
  (if
    (/= (getvar "insunits") 6)
    (progn
      (initget 0 "Sim Não")
      (setq key
            (getkword
              "\nO desenho não está em metros. Deseja corrigir isso? (pode-se mudar depois pelo comando _UNITS) [Sim/Não] <Sim>: "
            )
      )
      (if
        (member key '(nil "Sim"))
        (progn
          (setvar "INSUNITS" 6)
          (prompt "\nUnidade: metros.")
        )
        (prompt "\nA unidade não foi alterada!")
      )
    )
    (prompt "\nUnidade: metros.")
  )
)

(ad:check-dwg)   ; execute


;;; EOF
