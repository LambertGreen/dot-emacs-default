;; init-version-control.el --- -*- lexical-binding: t; -*-


;;; Version Control
(use-package vc
  :ensure nil
  :custom (vc-follow-symlinks t))

;;; Tansient
(use-package transient)

;;; Magit
;; Git porcelain inside Emacs
(use-package magit
  :after (general transient)
  :commands (magit-status magit-blame)
  :custom
  (magit-diff-refine-hunk t)
  (magit-save-repository-buffers 'dontask)
  ;; TODO Not sure of impact of `magit-auto-revert-mode' on large repo
  (magit-auto-revert-mode t)
  (magit-diff-hide-trailing-cr-characters t)
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  :init
;;;; Keymaps
  (lgreen/leader-define-key
    "g g" '(magit-status :wk "status")
    "g b" '(magit-blame :wk "blame")))

;;; Magit-Todos
;; There is always more todo
(use-package magit-todos
  :after magit
  :if (not (eq system-type 'windows-nt))
  :commands magit-todos-mode
  :custom
  (magit-todos-exclude-globs '(".git/" "*.min.js" "*.log" "node_modules/*"))
  :hook (magit-mode . magit-todos-mode))

;;; Diff-Hl
;; Show git status in the fringe
(use-package diff-hl
  :commands (diff-hl-previous-hunk diff-hl-next-hunk)
  :hook
  ((focus-in . diff-hl-update)
   (after-init . global-diff-hl-mode))
  :init
;;;; Keymaps
  (lgreen/leader-define-key
    "g p" '(diff-hl-previous-hunk :wk "previous hunk")
    "g n" '(diff-hl-next-hunk :wk "next hunk")))

;;; _
(provide 'init-version-control)
