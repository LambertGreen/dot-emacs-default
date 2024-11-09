;; init-terminal.el --- -*- lexical-binding: t; -*-

;;; Vterm
;; Skynet started somewhere
(use-package vterm
  :if (not (eq system-type 'windows-nt))
  :custom (vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=yes"))

;;; VTerm-toggle
;; I'll be back
(use-package vterm-toggle
  :after general
  :commands (vterm-toggle)
  :custom
  (vterm-toggle-fullscreen-p nil)
  (vterm-toggle-scope 'project)
  :init
;;;; Keymaps
  (lgreen/leader-define-key
    "o t" '(vterm-toggle :wk "Open vterm")))

;;; EAT
;; Emulate A Terminal
;;
;; NOTE: Disabled: Vterm copy/paste binds and reliability is better
;; NOTE: On first use you may need to run `eat-compile-terminfo'
(use-package eat
  :disabled t
  :after project
  :hook ((eat-mode . lgreen/eat-startup-in-line-mode)
         (kill-buffer . close-window-on-eat-buffer-kill))
  :bind ([remap project-shell] . eat-project)
  :custom (eat-kill-buffer-on-exit t)
  :init
;;;; Keymaps
  (lgreen/leader-define-key
    "o t" '(eat :wk "Open term")
    "p s" '(eat-project :wk "Shell in project"))
;;;; Functions
  (defun lgreen/eat-startup-in-line-mode ()
    "Start `eat' in line edit mode."
    (when (derived-mode-p 'eat-mode)
      (eat-line-mode)))
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

;; Local Variables:
;; jinx-local-words: "Skynet defun lgreen vterm"
;; End:
