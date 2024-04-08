;;; init-window-management.el --- -*- lexical-binding: t; -*-

;;; Winner mode
(use-package winner
  :ensure nil
  :hook (after-init . winner-mode)
  :init
  (lgreen/leader-keys
    "w u" '(winner-undo :wk "Window undo")))

;;; _
(provide 'init-window-management)
