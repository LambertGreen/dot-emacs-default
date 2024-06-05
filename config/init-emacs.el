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
  (global-auto-revert-non-file-buffers t)

  :config
  ;; Set custom file so that customizations are not written here
  (setq custom-file (make-temp-file "emacs-custom"))

  ;; Font
  (set-face-attribute 'default nil :font "Iosevka Nerd Font Mono" :height 128)

  ;; Visuals
  (scroll-bar-mode -1)        ; Disable visible scrollbar
  (tool-bar-mode -1)          ; Disable the toolbar
  (tooltip-mode -1)           ; Disable tooltips
  (set-fringe-mode 3)        ; Give some breathing room

  ;; Better find and grep
  ;; TODO Investigation actual usage of these grep commands
  (setq grep-command "rg --no-heading --color=never ")
  (setq grep-find-command "rg --files ")

  (global-auto-revert-mode 1))

;;; GCMH
;; the GC magic hack
;; TODO Evaluate the final removal of this package
;; (use-package gcmh
;;   :hook (after-init . gcmh-mode))

;;; No-Littering
;; Let's put the mess in './var'
(use-package no-littering
  :config (no-littering-theme-backups))

;;; Exec-Path-From-Shell
;; Get on the right $PATH
;; TODO Read the docs (on github) on how to use this is a performant manner
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
  ;; (which-key-dont-use-unicode t)
  (which-key-sort-order #'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 6)
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
  (lgreen/leader-define-key
    "h f" '(helpful-callable :wk "Describe function")
    "h v" '(helpful-variable :wk "Describe variable")
    "h k" '(helpful-key :wk "Describe key")
    "h c" '(helpful-command :wk "Describe command")))

;;; Rainbow-Delimiters
;; Highlight delimiters according to their depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;; Smartparens
;; Automatically balance inserting of parens
(use-package smartparens
  :after org
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  ;; Disable auto-pairing of single quotes in emacs-lisp-mode
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil))

;;; Dtrt-Indent
;; Guess file indentation
(use-package dtrt-indent
  :config (dtrt-indent-mode 1))

;;; Whitespace Cleanup
;; TODO Validate this package indeed works as advertised
(use-package whitespace-cleanup-mode
  :hook (prog-mode . whitespace-cleanup-mode))

;;; Ripgrep in Emacs
;; -- "What was lost is now found"
(use-package rg
  :config (rg-enable-default-bindings))

;;; Affe
;; An elisp FZF clone
(use-package affe)

;;; Nyan Mode
(use-package nyan-mode
  :init (nyan-mode))

;;; Dumb-Jump
;; Give it your best shot
(use-package dumb-jump)

;;; _
(provide 'init-emacs)
