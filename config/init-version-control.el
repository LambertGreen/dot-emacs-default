;;; init-version-control.el --- -*- lexical-binding: t; -*-


;;; Magit
;; Git porcelain inside Emacs
(use-package magit
  :after general
  :custom
  (magit-diff-refine-hunk t)
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  :init
  (lgreen/leader-keys
    "g g" '(magit-status :wk "Status")))

;;; Diff-Hl
;; Show git status in the fringe
(use-package diff-hl
  :after magit
  :init
  (lgreen/leader-keys
    "g p" '(diff-hl-previous-hunk :wk "Previous hunk")
    "g n" '(diff-hl-next-hunk :wk "Next hunk"))
  :config
  (global-diff-hl-mode)
  :hook
  (focus-in . diff-hl-update))

;;; _
(provide 'init-version-control)
