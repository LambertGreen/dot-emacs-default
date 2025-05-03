;; init-terminal.el --- -*- lexical-binding: t; -*-

;;; Vterm
;; Skynet started somewhere
;; NOTE: Disabling vterm due to color issue
(use-package vterm
  :disabled t
  :unless (eq system-type 'windows-nt)
  :custom (vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=yes")
  :hook (vterm-mode . #'lgreen/set-shell-theme-env))

;;; VTerm-toggle
;; I'll be back
(use-package vterm-toggle
  :after (general vterm)
  :commands (vterm-toggle)
  :custom
  (vterm-toggle-fullscreen-p nil)
  (vterm-toggle-scope 'project)
  :hook (vterm-mode . (lambda ()
                        (setq-local face-font-rescale-alist
                                    '(("Symbols Nerd Font Mono" . 0.8)))))
  :init
;;;; Keymaps
  (lgreen/leader-define-key
    "o t" '(vterm-toggle :wk "Open Vterm")))

;;; EAT
;; Emulate A Terminal
;; NOTE: On first use you may need to run `eat-compile-terminfo'
(use-package eat
  :after project
  :custom
  (eat-kill-buffer-on-exit t)
  :hook
  ((eat-mode . lgreen/set-shell-theme-env)
   (kill-buffer . lgreen/close-window-on-eat-buffer-kill)
   (eat-mode . (lambda ()
                 (setq-local face-font-rescale-alist
                             '(("Symbols Nerd Font Mono" . 0.8))))))
  :bind
  ([remap project-shell] . eat-project)
  (:map eat-mode-map
        ("s-v" . eat-yank)
        ("C-c C-y" . eat-yank))
  :init
;;;; Keymaps
  (lgreen/leader-define-key
    "o t" '(lgreen/toggle-eat :wk "Toggle EAT terminal")
    "o T" '(lgreen/new-eat :wk "New EAT terminal")
    "p s" '(eat-project :wk "Shell in project"))
;;;; Functions
  (defun lgreen/close-window-on-eat-buffer-kill ()
    "Close the window when an eat buffer is killed."
    (when (string-match-p "\\*.*eat\\*" (buffer-name))
      (delete-window)))
  (defun lgreen/new-eat ()
    "Create a new EAT terminal buffer."
    (interactive)
    (let ((eat-buffer-name (generate-new-buffer-name "*eat*")))
      (eat)))
  ;; Our manual sticky toggle
  (defvar lgreen/eat-last-location 'bottom
    "Last known location where EAT was opened: 'bottom or 'left.")
  (defun lgreen/toggle-eat ()
    "Toggle EAT terminal with simple remembered side (bottom/left)."
    (interactive)
    (let ((buf (get-buffer "*eat*")))
      (cond
       ;; 1. If EAT buffer is visible
       ((and buf (get-buffer-window buf))
        (let ((eat-win (get-buffer-window buf)))
          (setq lgreen/eat-last-location
                (if (window-combined-p eat-win)
                    'bottom
                  'left))
          (delete-window eat-win)))
       ;; 2. If EAT buffer exists but is not visible
       ((and buf (not (get-buffer-window buf)))
        (let ((win (if (eq lgreen/eat-last-location 'bottom)
                       (split-window-below)
                     (split-window-right))))
          (set-window-buffer win buf)
          (select-window win)))
       ;; 3. No EAT buffer exists yet
       (t
        ;; Do NOT manually split, just create eat normally
        (eat)))))
  :config
  ;; Optional: still default to bottom if manually opening EAT (only for first open)
  (add-to-list 'display-buffer-alist
               '("\\*.*eat\\*"
                 (display-buffer-at-bottom)
                 (window-height . 0.4))))

;;; _
(provide 'init-terminal)
