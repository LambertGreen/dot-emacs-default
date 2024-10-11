;; init-version-control.el --- -*- lexical-binding: t; -*-


;;; Emacs
(use-package emacs
  :ensure nil
  :custom (vc-follow-symlinks t))

(use-package transient)

;;; Magit
;; Git porcelain inside Emacs
(use-package magit
  :after (general transient)
  :custom
  (magit-diff-refine-hunk t)
  (magit-save-repository-buffers 'dontask)
  (magit-diff-hide-trailing-cr-characters t)
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  :init
  (lgreen/leader-define-key
    "g g" '(magit-status :wk "Status")))

;;; Magit-Todos
;; There is always more todo
(use-package magit-todos
  :after magit
  :if (not (eq system-type 'windows-nt))
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

;; _
(provide 'init-version-control)
