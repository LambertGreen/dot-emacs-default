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
  :disabled t  ; Disabled in favor of git-gutter
  :hook ((focus-in . diff-hl-update)
         (magit-post-refresh diff-hl-magit-post-refresh)
         (find-file . lgreen/setup-diff-hl-if-in-vcs))
  :custom (diff-hl-side 'right)
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

;;; Git-Gutter
;; Show git diff status in the margin (avoids fringe conflicts)
(use-package git-gutter
  :after general
  :custom
  ;; Update interval for git status checks
  (git-gutter:update-interval 0.5)
  ;; Width of the gutter in characters
  (git-gutter:window-width 2)
  ;; Visual indicators for changes
  (git-gutter:modified-sign "┃")
  (git-gutter:added-sign "┃")
  (git-gutter:deleted-sign "▁")
  ;; Hide gutter when there are no changes
  (git-gutter:hide-gutter t)
  :init
  ;; Keybindings for navigating diffs
  (lgreen/leader-define-key
    "g p" '(git-gutter:previous-hunk :wk "previous hunk")
    "g n" '(git-gutter:next-hunk :wk "next hunk")
    "g r" '(git-gutter:revert-hunk :wk "revert hunk")
    "g s" '(git-gutter:stage-hunk :wk "stage hunk")
    "t g" '(global-git-gutter-mode :wk "toggle git-gutter")
    "t G" '(git-gutter :wk "refresh git-gutter"))
  :config
  (global-git-gutter-mode 1)

  ;; Update git-gutter colors based on diff faces
  (defun lgreen/update-git-gutter-colors ()
    "Update git-gutter colors to match current theme."
    (set-face-foreground 'git-gutter:modified (face-foreground 'diff-changed))
    (set-face-foreground 'git-gutter:added (face-foreground 'diff-added))
    (set-face-foreground 'git-gutter:deleted (face-foreground 'diff-removed)))

  (lgreen/update-git-gutter-colors)
  (add-hook 'after-load-theme-hook #'lgreen/update-git-gutter-colors))

;;; Forge
;; The codesmith pounding the code into useful tools
(use-package forge
  :after magit
  :commands (forge-pull forge-list-pullreqs forge-list-issues)
  :custom
  (forge-add-default-bindings nil)  ; Add this line
  ;; Limit how many topics (PRs/issues) to fetch by default
  (forge-topic-list-limit '(60 . 0))
  ;; Only fetch notifications for repos you're watching
  (forge-owned-accounts '(("lambert-green_sfemu") ("LambertGreen")))
  :init
  ;; Keybindings for forge commands
  (lgreen/leader-define-key
    "g f " '(:ignore t :wk "Forge")
    "g f p" '(forge-pull :wk "pull")
    "g f l" '(forge-list-pullreqs :wk "list PRs")
    "g f i" '(forge-list-issues :wk "list issues"))
  :config
  ;; Map SSH aliases to GitHub
  (dolist (alias '("github.com-work" "github.com-personal"))
    (add-to-list 'forge-alist
                 `(,alias "api.github.com" "github.com" forge-github-repository))))

;;; _
(provide 'init-version-control)
