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
  :config (no-littering-theme-backups))

;;; Exec-Path-From-Shell
;; Set a useful $PATH
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

;;; Which-Key
;; Which key? This one.
(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-min-display-lines 3)
  (setq which-key-sort-uppercase-first nil))

;;; Consult
;; Making buffer completions nicer
(use-package consult
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
  :config
  (lgreen/leader-keys
    "f t" '(consult-todo :wk "Find todos")
    "f T" '(consult-todo-all :wk "Find all todos")
    "h t" '(consult-theme :wk "Switch theme")
    "s b" '(consult-line :wk "Search buffer")
    "s p" '(consult-ripgrep :wk "Search project files")
    "s i" '(consult-imenu :wk "Jump to symbol")
    "s d" '(consult-locate :wk "Search current directory")))

;;; Wgrep
;; Let's bulk update
(use-package wgrep)

;;; Savehist
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure nil
  :init (savehist-mode))

;;; Vertico
;; Veritcal completion UI
(use-package vertico
  :custom
  ;; Different scroll margin
  (vertico-scroll-margin 0)
  (vertico-count 20)
  (vertico-cycle t)
  :init
  (vertico-mode))

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
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;; Marginalia
;; Annotaions for minibuffer completions
(use-package marginalia
  :init (marginalia-mode))

;;; Embark
;; Run actions on item on point
(use-package embark
  :after (general)
  :general
  (general-define-key
   "C-;"  'embark-act
   "C-." 'embark-dwim)
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
  :config
  (lgreen/leader-keys
    "h f" '(helpful-callable :wk "Describe function")
    "h v" '(helpful-variable :wk "Describe variable")
    "h k" '(helpful-key :wk "Describe key")
    "h c" '(helpful-command :wk "Describe command")))

;;; Rainbow-Delimiters
;; Highlight delimiters according to their depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;; Undo-Tree
;; "What good is a mind if you can't change it"
(use-package undo-tree
  :custom
  (evil-undo-system 'undo-tree)
  (undo-tree-enable-undo-in-region t)
  :init (global-undo-tree-mode))

;;; Smartparens
;; Automatically balance inserting of parens
(use-package smartparens
  :config (smartparens-global-mode 1))

;;; Dtrt-Indent
;; Guess file indentation
(use-package dtrt-indent
  :config (dtrt-indent-mode 1))

;;; Whitespace Cleanup
;; TODO Validate this package indeed works as advertised
(use-package whitespace-cleanup-mode
  :hook (prog-mode . whitespace-cleanup-mode))

;;; _
(provide 'init-emacs)
