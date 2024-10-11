;; init-emacs.el --- -*- lexical-binding: t; -*-


;;; Emacs
(use-package emacs
  :ensure nil
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
  ;; Enable desktop save  mode
  ;; TODO I don't think I want this option enabled as I use multiple Emacs instances
  ;; and don't really want any of their window configurations saved, however I do want
  ;; recently used files remembered. I need to enable the option that keeps such history.
  ;; (desktop-save-mode 1)

;;;; Search programs
  ;; Better find and grep
  ;; TODO Investigation actual usage of these grep commands
  (setq grep-command "rg --no-heading --color=never ")
  (setq grep-find-command "rg --files ")

;;;; Custom file
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file 'noerror))

;;; No-Littering
;; Let's put the mess in './var'
(use-package no-littering
  :config
  (no-littering-theme-backups)
  ;; Store backup files in the no-littering directory
  (setq backup-directory-alist `(("." . ,(no-littering-expand-var-file-name "backups/"))))
  ;; Store auto-save files in the no-littering directory
  (setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  ;; Store lockfiles in the no-littering directory
  (setq lock-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "lockfiles/") t))))

;;; Persistent Scratch
(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

;;; Exec-Path-From-Shell
;; Get on the right $PATH
;; TODO Read the docs (on github) on how to use this in a performant manner
;; TODO Looks like this is not needed when launching Emacs via Alfred
(use-package exec-path-from-shell
  :disabled t
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

;;; Which-Key
;; Which key? This one.
;; NOTE Press "?" to disambiguate long entries with the same prefix
(use-package which-key
  :custom
  ;; Going with empty ellipsis to avoid alignment issues,
  ;; which is fine given the overlong items are clearly
  ;; up against the limit
  (which-key-ellipsis "") ;; options: "︙",
  ;; An alternate solution to the alignment issue
  ;; (which-key-don't-use-unicode t)
  (which-key-sort-order #'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 6)
  ;; Get Which-Key to always show at the bottom
  ;; NOTE: we don't use the below config because while it gets
  ;; the popup at the bottom, it does not show the useful help line
  ;; (which-key-popup-type 'minibuffer)
  ;; TODO I did not find a good quick fix to opening `which-key' while `eat' is open
  ;; ChatGPT gave some suggestions, but I did not have time to implement them
  (which-key-side-window-slot -10)
  :init (which-key-mode))

;;; Wgrep
;; Let's bulk update
(use-package wgrep)

;;; Savehist
;; Persist history over Emacs restarts.
(use-package savehist
  :ensure nil
  :after no-littering
  :init (savehist-mode))

;;; Orderless
;; Orderless completion style
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;; Embark
;; Take action for your responsibility
;; TODO Refactor: move to a file called "actions"
(use-package embark
  :after general
  ;; FIXME Find better embark keybinds as we are using "C-;" for completion
  ;; FIXME Find better embark keybinds as we are using "C-'" for toggling terminal window
  ;; :general
  ;; (evil-normal-state-map
  ;;  "C-'"  'embark-act
  ;;  "C-." 'embark-dwim)
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  (lgreen/leader-define-key
    "a" '(embark-act :wk "Actions")
    "A" '(embark-dwim :wk "Actions-dwim")
    "h B" '(embark-bindings :wk "Embark bindings"))

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;;; Embark-Consult
;; Integration between 'embark' and 'consult'
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; Helpful
;; A better *help* buffer
(use-package helpful
  :after general
  :init
  (setq evil-lookup-func #'helpful-at-point)
  (lgreen/leader-define-key
    "h f" '(helpful-function :wk "Describe function")
    "h v" '(helpful-variable :wk "Describe variable")
    "h k" '(helpful-key :wk "Describe key")
    "h c" '(helpful-command :wk "Describe command")
    "h C" '(helpful-callable :wk "Describe callable")
    "h s" '(helpful-symbol :wk "Describe symbol")
    )
  :bind
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-function)
  ([remap describe-variable] . helpful-variable))

;;; Rainbow-Delimiters
;; Highlight delimiters according to their depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;; Smartparens
;; Automatically balance inserting of parens
(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  ;; Disable auto-pairing of single quotes in emacs-lisp-mode
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil))

;;; Ripgrep in Emacs
;; -- "What was lost is now found"
(use-package rg
  :config (rg-enable-default-bindings))

;;; Affe
;; An elisp FZF clone
(use-package affe)

;;; Nyan Mode
;; Rainbow and cat
(use-package nyan-mode
  :init
  (when (display-graphic-p)
    (nyan-mode)))

;;; Dumb-Jump
;; Give it your best shot
(use-package dumb-jump)

;;; Proced
;; Reach for this before htop/btop/etc.
(use-package proced
  :ensure nil
  :custom (proced-enable-color-flag t))

;; _
(provide 'init-emacs)

;; Local Variables:
;; jinx-local-words: "Aile Eldoc Iosevka Orderless Propo btop dwim eldoc elisp emacs github htop lockfiles minibuffer modeline parens rg setq tooltips unicode whitespace"
;; End:
