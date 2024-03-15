;;; init-term.el --- -*- lexical-binding: t; -*-

;;; Vterm
(use-package vterm)

;;; VTerm-toggle
(use-package vterm-toggle
  :config
  (lgreen/leader-keys
      "o t" '(vterm-toggle :wk "Open vterm")))

;;; EAT
(use-package eat)

;;; _
(provide 'init-term)
