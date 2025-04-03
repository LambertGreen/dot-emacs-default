;; init-terminal.el --- -*- lexical-binding: t; -*-

;;; Vterm
;; Skynet started somewhere
;; TODO Disabling Vterm because color is now getting messed up
(use-package vterm
  :unless (eq system-type 'windows-nt)
  :disabled t
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
  :hook (vterm-mode . (lambda () (setq-local face-font-rescale-alist
                                             '(("Symbols Nerd Font Mono" . 0.8)))))
  :init
;;;; Keymaps
  (lgreen/leader-define-key
    "o t" '(vterm-toggle :wk "Open Vterm")))

;;; EAT
;; Emulate A Terminal
;; NOTE: Disabled: Vterm copy/paste binds and reliability is better
;; NOTE: On first use you may need to run `eat-compile-terminfo'
(use-package eat
  :after project
  :custom (eat-kill-buffer-on-exit nil)
  :hook ((eat-mode . lgreen/set-shell-theme-env)
         (kill-buffer . close-window-on-eat-buffer-kill)
         (eat-mode . (lambda () (setq-local face-font-rescale-alist
                                            '(("Symbols Nerd Font Mono" . 0.8))))))
  :bind ([remap project-shell] . eat-project)
  ;; Add s-v and fallback binding inside eat-mode
  :bind (:map eat-mode-map
              ("s-v" . eat-yank)
              ("C-c C-y" . eat-yank))
  :custom (eat-kill-buffer-on-exit t)
  :init
;;;; Keymaps
  (lgreen/leader-define-key
    "o t" '(eat :wk "Open term")
    "p s" '(eat-project :wk "Shell in project"))
;;;; Functions
  (defun close-window-on-eat-buffer-kill ()
    "Close the window when an eat buffer is killed."
    (when (string-match-p "\\*.*eat\\*" (buffer-name))
      (delete-window)))
  :config
  ;; Make terminal (eat) buffer show in right window
  (add-to-list 'display-buffer-alist
               '("\\*.*eat\\*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (display-buffer-reuse-window
                  display-buffer-at-bottom)
                 (window-height . 0.4))))

;;; _
(provide 'init-terminal)
