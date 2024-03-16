;;; init-terminal.el --- -*- lexical-binding: t; -*-

;;; Vterm
(use-package vterm)

;;; VTerm-toggle
(use-package vterm-toggle
  :after general
  :init
  (lgreen/leader-keys
      "o t" '(vterm-toggle :wk "Open vterm")))

;;; EAT
(use-package eat)

;;; _
(provide 'init-terminal)
