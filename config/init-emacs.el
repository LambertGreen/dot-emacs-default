;;; init-emacs.el --- -*- lexical-binding: t; -*-

(use-package emacs
  :ensure nil
  :config

  ;; Set custom file so that customizations are not written here
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file)

  ;; Set personal info
  (setq user-full-name "Lambert Green"
	user-mail-address "lambert.green@gmail.com")

  ;; Font
  (set-face-attribute 'default nil :font "Iosevka Nerd Font" :height 140)

  ;; Visuals
  (scroll-bar-mode -1)        ; Disable visible scrollbar
  (tool-bar-mode -1)          ; Disable the toolbar
  (tooltip-mode -1)           ; Disable tooltips
  ;; (set-fringe-mode 4)        ; Give some breathing room

  (setq use-short-answers t)

  ;; Save the desktop upon exit
  (desktop-save-mode 1))

(use-package no-littering
  :ensure t
  :config
  (no-littering-theme-backups))

;; Set a useful $PATH
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config
  (setq which-key-min-display-lines 3)
  (setq which-key-sort-uppercase-first nil))

(use-package consult
  :ensure t
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
	register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Enable vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  ;; Different scroll margin
  (setq vertico-scroll-margin 0)
  ;; Show more candidates
  (setq vertico-count 20)
  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)
  (setq vertico-cycle t))

;; A few more useful configurations...
(use-package emacs
  :ensure nil
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
		  (replace-regexp-in-string
		   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
		   crm-separator)
		  (car args))
	  (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; TODO Evaluate if you want to keep this setting
  (setq read-extended-command-predicate
	#'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
	      ("M-A" .
	       marginalia-cycle))

  :init (marginalia-mode))

(use-package helpful
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; use the powerful 'undo-tree'
;; "What good is a mind if you can't change it"
(use-package undo-tree
  :ensure t
  :custom
  (evil-undo-system 'undo-tree)
  :init
  (global-undo-tree-mode))

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode 1))

;;; _
(provide 'init-emacs)
