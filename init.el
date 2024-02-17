;;; init.el --- -*- lexical-binding: t; -*-

;; Use Elpaca for our package manager
(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
				:ref nil
				:files (:defaults "elpaca-test.el" (:exclude "extensions"))
				:build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
	(build (expand-file-name "elpaca/" elpaca-builds-directory))
	(order (cdr elpaca-order))
	(default-directory repo))
    (add-to-list 'load-path (if (file-exists-p build) build repo))
    (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
	(if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
		((zerop (call-process "git" nil buffer t "clone"
					(plist-get order :repo) repo)))
		((zerop (call-process "git" nil buffer t "checkout"
					(or (plist-get order :ref) "--"))))
		(emacs (concat invocation-directory invocation-name))
		((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
					"--eval" "(byte-recompile-directory \".\" 0 'force)")))
		((require 'elpaca))
		((elpaca-generate-autoloads "elpaca" repo)))
	    (progn (message "%s" (buffer-string)) (kill-buffer buffer))
	    (error "%s" (with-current-buffer buffer (buffer-string))))
	((error) (warn "%s" err) (delete-directory repo 'recursive))))
    (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
    ;; Enable use-package :ensure support for Elpaca.
    (elpaca-use-package-mode))

;; Block until current queue processed.
(elpaca-wait)

(use-package general
  :ensure t
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer lgreen/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  (lgreen/leader-keys
    "SPC" '(execute-extended-command :wk "M-x")
    "." '(find-file :wk "find-file"))

  ;; find
  (lgreen/leader-keys
    "f" '(:ignore t :wk "file")
    "f f" '(find-file :wk "Find file")
    "f r" '(recentf :wk "Recent files")
    "f c" '((lambda () (interactive) (find-file "~/.emacs.default/README.org")) :wk "Edit emacs config"))

  ;; Buffers
  (lgreen/leader-keys
    "b" '(:ignore t :wk "buffer")
    "b b" '(switch-to-buffer :wk "Switch buffer")
    "b d" '(kill-this-buffer :wk "Delete buffer")
    "b n" '(next-buffer :wk "Next buffer")
    "b N" '(evil-buffer-new :wk "New buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer")
    "b R" '(rename-buffer :wk "Rename buffer")
    "b s" '(save-buffer :wk "Save buffer")
    "b S" '(evil-write-all :wk "Save all buffers")
    )

  ;; Windows
  (lgreen/leader-keys
    "w" '(:ignore t :wk "window")
    "w =" '(balance-windows :wk "Balance windows")
    "w h" '(evil-window-left :wk "Window left")
    "w k" '(evil-window-up :wk "Window up")
    "w j" '(evil-window-down :wk "Window down")
    "w l" '(evil-window-right :wk "Window right")
    "w q" '(evil-quit :wk "Window close")
    "w d" '(delete-window :wk "Delete window")
    "w s" '(split-window-below :wk "Split window below")
    "w v" '(split-window-right :wk "Split window right")
    "w o" '(delete-other-windows :wk "Delete other windows")
    "w f" '(toggle-frame-fullscreen :wk "Toggle fullscreen")
    "w m" '(toggle-frame-maximized :wk "Toggle maximized")
    "w H" '(evil-window-move-far-left :wk "Move window far left")
    "w K" '(evil-window-move-very-top :wk "Move window very top")
    "w J" '(evil-window-move-very-bottom :wk "Move window very bottom")
    "w L" '(evil-window-move-far-right :wk "Move window far right"))

  ;; Quit
  (lgreen/leader-keys
    "q" '(:ignore t :wk "quit")
    "q q" '(save-buffers-kill-terminal :wk "Quit"))


  ;; Toggles
  (lgreen/leader-keys
    "t" '(:ignore t :wk "toggle")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t w" '(visual-line-mode :wk "Toggle truncated lines"))
  )

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq org-return-follows-link t)
  :config
  ;; Use Emacs keybindings in insert mode for C-a and C-e
  (define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
  (evil-set-initial-state 'eat-mode 'insert)
  (evil-mode))

;; vim-like keybindings everywhere in emacs
(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init)
  ;; Unmap keys in 'evil-maps. If not done, (setq org-return-follows-link t) will not work
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "SPC") nil)
    (define-key evil-motion-state-map (kbd "RET") nil)
    (define-key evil-motion-state-map (kbd "TAB") nil)))

;; gl and gL operators, like vim-lion
(use-package evil-lion
  :ensure t
  :bind (:map evil-normal-state-map
	      ("gl " . evil-lion-left)
	      ("gL " . evil-lion-right)
	      :map evil-visual-state-map
	      ("gl " . evil-lion-left)
	      ("gL " . evil-lion-right)))

;; gc operator, like vim-commentary
(use-package evil-commentary
  :ensure t
  :after evil
  :config (evil-commentary-mode))

;; gx operator, like vim-exchange
;; NOTE using cx like vim-exchange is possible but not as straightforward
(use-package evil-exchange
  :ensure t
  :bind (:map evil-normal-state-map
	      ("gx" . evil-exchange)
	      ("gX" . evil-exchange-cancel)))

;; gr operator, like vim's ReplaceWithRegister
(use-package evil-replace-with-register
  :ensure t
  :bind (:map evil-normal-state-map
	      ("gr" . evil-replace-with-register)
	      :map evil-visual-state-map
	      ("gr" . evil-replace-with-register)))

;; * operator in visual mode
(use-package evil-visualstar
  :ensure t
  :bind (:map evil-visual-state-map
	      ("*" . evil-visualstar/begin-search-forward)
	      ("#" . evil-visualstar/begin-search-backward)))

;; ex commands, which a vim user is likely to be familiar with
(use-package evil-expat :ensure t)

;; visual hints while editing
(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-use-diff-faces)
  (evil-goggles-mode))

;; like vim-surround
(use-package evil-surround
  :ensure t
  :commands
  (evil-surround-edit
   evil-Surround-edit
   evil-surround-region
   evil-Surround-region)
  :init
  (evil-define-key 'operator global-map "s" 'evil-surround-edit)
  (evil-define-key 'operator global-map "S" 'evil-Surround-edit)
  (evil-define-key 'visual global-map "S" 'evil-surround-region)
  (evil-define-key 'visual global-map "gS" 'evil-Surround-region))

(use-package emacs
    :ensure nil
    :config

    ;; Set personal info
    (setq user-full-name "Lambert Green"
	user-mail-address "lambert.green@gmail.com")

    ;; Font
    (set-face-attribute 'default nil :font "Iosevka Nerd Font" :height 140)

    ;; Visuals
    (scroll-bar-mode -1)        ; Disable visible scrollbar
    (tool-bar-mode -1)          ; Disable the toolbar
    (tooltip-mode -1)           ; Disable tooltips
    (set-fringe-mode 10)        ; Give some breathing room

    ;; Set custom file so that customizations are not written here
    (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
    (load custom-file))

(use-package no-littering
  :ensure t
  :config
  (no-littering-theme-backups))

;; Get a beautiful and functional theme
(use-package catppuccin-theme
  :ensure t
  :config (load-theme 'catppuccin :no-confirm))

;; Add Doom's modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 20)))

;; Let's try this other modeline shall we?
;; (use-package telephone-line
;;   :init (telephone-line-mode 1))

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
  :after general
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

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  (lgreen/leader-keys
    "s" '(:ignore t :wk "Search")
    "s b" '(consult-line :wk "Search buffer")
    "s p" '(consult-ripgrep :wk "Search project files")
    "s i" '(consult-imenu :wk "Jump to symbol")
    "s d" '(consult-locate :wk "Search current directory")))

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

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

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
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

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
	      ("M-A" . marginalia-cycle))

  :init (marginalia-mode))

(use-package helpful
  :ensure t
  :after general
  :config
  (lgreen/leader-keys
    "h" '(:ignore t :wk "help")
    "h f" '(helpful-callable :wk "Describe function")
    "h v" '(helpful-variable :wk "Describe variable")
    "h k" '(helpful-key :wk "Describe key")
    "h x" '(helpful-command :wk "Describe command")))

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

(use-package yasnippet-snippets
  :ensure t
  :hook (prog-mode . yas-minor-mode))

(use-package org-bullets
  :ensure t
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package evil-org
  :ensure t
  :after org
  :hook ((org-mode . evil-org-mode)
	 (evil-org-mode . evil-org-set-key-theme))
  :config
  (evil-define-key 'normal evil-org-mode-map
    (kbd "M-k") #'org-metaup
    (kbd "M-j") #'org-metadown))

(use-package projectile
  :ensure t
  :after general
  :config
  (projectile-mode +1)
  (setq projectile-project-search-path '(("~/dev" . 7)))
  (lgreen/leader-keys
    "p" '(:ignore t :wk "project")
    "p p" '(projectile-switch-project :wk "Switch project")
    "p f" '(projectile-find-file :wk "Find file in project")
    "p d" '(projectile-dired :wk "Dired in project")
    "p b" '(projectile-switch-to-buffer :wk "Switch buffer in project")))

(use-package eat
  :ensure t)

(use-package lorem-ipsum :ensure t)

(use-package dired-narrow :ensure t)
(use-package origami :ensure t)
(use-package company :ensure t)
