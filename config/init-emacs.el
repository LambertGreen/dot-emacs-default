;; init-emacs.el --- -*- lexical-binding: t; -*-

;;; No-Littering
;; Let's put the mess in './var'
(use-package no-littering
  :demand t
  :init
  ;; Store backup files in the no-littering directory
  (setq backup-directory-alist `(("." . ,(no-littering-expand-var-file-name "backups/"))))
  ;; Store auto-save files in the no-littering directory
  (setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  ;; Store lockfiles in the no-littering directory
  (setq lock-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "lockfiles/") t)))
  :config
  (no-littering-theme-backups))

;;; Emacs
(use-package emacs
  :ensure nil
  :demand t
  :custom
  ;; Set personal info
  (user-full-name "Lambert Green")
  (user-mail-address "lambert.green@gmail.com")

  (use-short-answers t)
  (auto-revert-check-vc-info t)
  (global-auto-revert-non-file-buffers t)

  ;; Default text settings
  (indent-tabs-mode nil)
  (tab-width 4)

  :config
;;;; Fonts
  ;; Default font
  (if (eq system-type 'darwin)
      (set-face-attribute 'default nil :font "Iosevka Nerd Font" :height 128))
  (if (eq system-type 'windows-nt)
      (set-face-attribute 'default nil :font "Iosevka NF" :height 128))

  ;; Fixed-Pitch font
  (if (eq system-type 'darwin)
      (set-face-attribute 'fixed-pitch nil :family "Iosevka Nerd Font Mono"))
  (if (eq system-type 'windows-nt)
      (set-face-attribute 'fixed-pitch nil :family "Iosevka NFM"))

  ;; Variable-Pitch font
  (if (eq system-type 'darwin)
      (set-face-attribute 'variable-pitch nil :family "Iosevka Nerd Font Propo"))
  (if (eq system-type 'windows-nt)
      (set-face-attribute 'variable-pitch nil :family "Iosevka NFP"))

;;;; Visuals
  (when (display-graphic-p)
    (scroll-bar-mode -1)
    )
  (if (not (eq system-type 'darwin))
      (menu-bar-mode -1))
  (tool-bar-mode -1)
  (tooltip-mode -1)
  ;; Make the frame title include the project name
  ;; Allows for easy switching to Emacs frame by project name
  (setq frame-title-format
        '(""
          "%b"
          (:eval
           (let ((project (project-current)))
             (when project
               (let ((project-name (file-name-nondirectory (directory-file-name (project-root project)))))
                 (format " in [%s] - Emacs" project-name)))))))

;;;; Auto Save
  (auto-save-visited-mode 1)

;;;; Auto Revert
  (global-auto-revert-mode 1)

  ;;;; Recentf
  (recentf-mode 1)

;;;; Search programs
  ;; Better find and grep
  ;; TODO Investigation actual usage of these grep commands
  (setq grep-command "rg --no-heading --color=never ")
  (setq grep-find-command "rg --files ")

;;;; Custom file
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file 'noerror))

;;; _
(provide 'init-emacs)

;; Local Variables:
;; jinx-local-words: "Aile Eldoc Iosevka Orderless Propo btop dwim eldoc elisp emacs github htop lockfiles minibuffer modeline parens rg setq tooltips unicode whitespace"
;; End:
