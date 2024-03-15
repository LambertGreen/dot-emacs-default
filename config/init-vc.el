;;; init-vc.el --- -*- lexical-binding: t; -*-


;;; Magit
;; Git porcelain inside Emacs
(use-package magit
  :config
  (setq magit-diff-refine-hunk t)
  :hook (magit-post-refresh-hook . diff-hl-magit-post-refresh))

;;; Diff-Hl
;; Show git status in the fringe
(use-package diff-hl
  :config
  (global-diff-hl-mode))

;;; _
(provide 'init-vc)
