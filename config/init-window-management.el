;;; init-window-management.el --- -*- lexical-binding: t; -*-

;;; Winner mode
(use-package winner
  :ensure nil
  :hook (after-init . winner-mode)
  :init
  (lgreen/leader-keys
    "w r" 'winner-redo
    "w u" 'winner-undo))

;;; _
(provide 'init-window-management)
