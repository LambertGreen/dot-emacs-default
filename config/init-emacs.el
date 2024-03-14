;;; init-emacs.el --- -*- lexical-binding: t; -*-

;;; Emacs
(use-package emacs
  :ensure nil
  :custom
  ;; Set personal info
  (user-full-name "Lambert Green")
  (user-mail-address "lambert.green@gmail.com")

  (use-short-answers t)
  (auto-revert-check-vc-info t)

  :config
  ;; Set custom file so that customizations are not written here
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file)

  ;; Font
  (set-face-attribute 'default nil :font "Iosevka Nerd Font" :height 140)

  ;; Visuals
  (scroll-bar-mode -1)        ; Disable visible scrollbar
  (tool-bar-mode -1)          ; Disable the toolbar
  (tooltip-mode -1)           ; Disable tooltips
  (set-fringe-mode 3)        ; Give some breathing room

  (global-auto-revert-mode 1))

;;; No-Littering
;; Let's put the mess in './var'
(use-package no-littering
  :ensure t
  :config
  (no-littering-theme-backups))

;;; Exec-Path-From-Shell
;; Set a useful $PATH
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

;;; Which-Key
;; Which key? This one.
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config
  (setq which-key-min-display-lines 3)
  (setq which-key-sort-uppercase-first nil))

;;; Consult
;; Making buffer completions nicer
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

;;; Wgrep
;; Let's bulk update
(use-package wgrep
  :ensure t)

;;; Savehist
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;;; Vertico
;; Veritcal completion UI
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

;;; Orderless
;; Orderless completion style
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;; Marginalia
;; Annotaions for minibuffer completions
(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
	      ("M-A" .
	       marginalia-cycle))

  :init (marginalia-mode))

;;; Embark
;; Run actions on item on point
(use-package embark
  :ensure t
  :after (general)
  :bind
  (("C-;" . embark-act)         ;; pick some comfortable binding
   ("C-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
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

  (lgreen/leader-keys
    "a" '(embark-act :wk "Actions")
    "A" '(embark-dwim :wk "Actions-dwim"))

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

;;; Embark-Consult
;; Integration between 'embark' and 'consult'
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; Helpful
;; A better *help* buffer
(use-package helpful
  :ensure t)

;;; Rainbow-Delimiters
;; Highlight delimiters according to their depth
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;;; Undo-Tree
;; "What good is a mind if you can't change it"
(use-package undo-tree
  :ensure t
  :custom
  (evil-undo-system 'undo-tree)
  (undo-tree-enable-undo-in-region t)
  :init
  (global-undo-tree-mode))

;;; Smartparens
;; Automatically balance inserting of parens
(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode 1))

;;; Dtrt-Indent
;; Guess file indentation
(use-package dtrt-indent
  :ensure t
  :config
  ;; enable dtrt-indent-mode globally
  (dtrt-indent-mode 1))

;;; Whitespace Cleanup
;; TODO Validate this package indeed works as advertised
(use-package whitespace-cleanup-mode
  :ensure t
  :hook
  (prog-mode . whitespace-cleanup-mode))

;;; _
(provide 'init-emacs)
