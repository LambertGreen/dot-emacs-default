;;; init-terminal.el --- -*- lexical-binding: t; -*-

;;; Vterm
(use-package vterm)

;;; VTerm-toggle
(use-package vterm-toggle
  :after general
  :custom
  (vterm-toggle-fullscreen-p nil)
  (vterm-toggle-scope 'project)
  :init
  (lgreen/leader-define-key
    "o t" '(vterm-toggle :wk "Open vterm"))
  )

;;; _
(provide 'init-terminal)
