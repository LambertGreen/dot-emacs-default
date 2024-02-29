;;; init-vc.el --- -*- lexical-binding: t; -*-

(use-package magit
  :ensure t
  :config
  (setq magit-diff-refine-hunk t))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))

;;; _
(provide 'init-vc)
