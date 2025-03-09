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
  :hook (magit-status-mode . (lambda () (toggle-truncate-lines)))
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
  :unless (eq system-type 'windows-nt)
  :commands magit-todos-mode
  :custom
  (magit-todos-exclude-globs '(".git/" "*.min.js" "*.log" "node_modules/*"))
  :hook (magit-mode . magit-todos-mode))

;;; Diff-Hl
;; Show diff status in the fringe
(use-package diff-hl
  :hook
  ((focus-in . diff-hl-update)
   (find-file . lgreen/setup-diff-hl-if-in-vcs))
  :init
  ;; Keybindings for navigating diffs
  (lgreen/leader-define-key
    "g p" '(diff-hl-previous-hunk :wk "previous hunk")
    "g n" '(diff-hl-next-hunk :wk "next hunk"))

  ;; Function to set up diff-hl in VCS projects
  (defun lgreen/setup-diff-hl-if-in-vcs ()
    "Enable diff-hl if the current project is under version control."
    (let ((project (project-current t)))
      (when (and project
                 (eq (car project) 'vc)) ; Check if project is a VCS project
        (require 'diff-hl)
        (global-diff-hl-mode)))))

;;; _
(provide 'init-version-control)
