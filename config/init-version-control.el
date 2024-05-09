;;; init-version-control.el --- -*- lexical-binding: t; -*-


;;; Magit
;; Git porcelain inside Emacs
(use-package magit
  :after general
  :custom
  (magit-diff-refine-hunk t)
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  :init
  (lgreen/leader-define-key
    "g g" '(magit-status :wk "Status")))

;;; Magit-Todos
;; There is always more todo
(use-package magit-todos
  :after magit
  :init
  (lgreen/leader-define-key
    "p t" '(magit-todos-list :wk "todos"))
  :config (magit-todos-mode 1))

;;; Diff-Hl
;; Show git status in the fringe
(use-package diff-hl
  :after magit
  :init
  (lgreen/leader-define-key
    "g p" '(diff-hl-previous-hunk :wk "Previous hunk")
    "g n" '(diff-hl-next-hunk :wk "Next hunk"))
  :config
  (global-diff-hl-mode)
  :hook
  (focus-in . diff-hl-update))

;;; _
(provide 'init-version-control)
