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
(use-package eat
  :after evil
  :config
  ;; Set the initial state for specific modes, if necessary
  (evil-set-initial-state 'eat-mode 'insert))

;;; _
(provide 'init-terminal)
