;;; init-vc.el --- -*- lexical-binding: t; -*-


;;; Magit
;; Git porcelain inside Emacs
(use-package magit
  :ensure t
  :config
  (setq magit-diff-refine-hunk t))

;;; Diff-Hl
;; Show git status in the fringe
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))

;;; _
(provide 'init-vc)
