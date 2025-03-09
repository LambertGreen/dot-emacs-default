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

  :init
  ;; Default text settings
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq-default truncate-lines t)

  :config
;;;; Fonts
  ;; Default, Fixed-Pitch, and Variable-Pitch fonts based on system type
  ;; TODO: Update to using `Aporetic' font for `Windows' and `Linux'
  (when (display-graphic-p)
    (cond
     ((eq system-type 'darwin)
      (set-face-attribute 'default nil :font "Aporetic Sans Mono" :height 132)
      (set-face-attribute 'fixed-pitch nil :family "Aporetic Sans Mono")
      (set-face-attribute 'variable-pitch nil :family "Aporetic Sans Mono")
      (set-fontset-font t 'unicode "Symbols Nerd Font Mono" nil 'append))
     ((eq system-type 'linux)
      (set-face-attribute 'default nil :font "Iosevka Nerd Font" :height 132)
      (set-face-attribute 'fixed-pitch nil :family "Iosevka Nerd Font Mono")
      (set-face-attribute 'variable-pitch nil :family "Iosevka Nerd Font Propo")
      (set-fontset-font t 'unicode "Symbols Nerd Font Mono" nil 'append))
     ((eq system-type 'windows-nt)
      (set-face-attribute 'default nil :font "Iosevka NF" :height 132)
      (set-face-attribute 'fixed-pitch nil :family "Iosevka NFM")
      (set-face-attribute 'variable-pitch nil :family "Iosevka NFP")
      (set-fontset-font t 'unicode "Symbols Nerd Font Mono" nil 'append)))

    ;; Needed to have icons in vterm be correct (overrides file-icons)
    (set-fontset-font t '(#xf000 . #xf1ff) "Symbols Nerd Font Mono" nil 'prepend))

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
        '("%b"
          (:eval
           (let* ((project (or (project-current)
                               ;; Fall back to tabspaces workspace name
                               (when (bound-and-true-p tabspaces-mode)
                                 (tabspaces--current-tab-name))))
                  (project-name (when project
                                  (if (stringp project)  ;; If it's from tabspaces
                                      project
                                    ;; If it's a project object, get the name
                                    (file-name-nondirectory (directory-file-name (project-root project)))))))
             (if project-name
                 (format " in [%s] - %s" project-name invocation-name)
               (format " - %s" invocation-name))))))

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

;;;; Ultra-Scroll
;; Making the touchpad relevant again
(use-package ultra-scroll
  :ensure (:fetcher github :repo "jdtsmith/ultra-scroll")
  :demand t
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

;;; _
(provide 'init-emacs)
