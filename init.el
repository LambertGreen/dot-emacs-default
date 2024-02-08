;;; init.el --- -*- lexical-binding: t; -*-

;; Disable native comp warnings showing as errors
(setq native-comp-async-report-warnings-errors nil)

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; Set personal info
(setq user-full-name "Lambert Green"
      user-mail-address "lambert.green@gmail.com")

;; Set custom file so that customizations are not written here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Font
(set-face-attribute 'default nil :font "Iosevka Nerd Font" :height 140)

;; Visuals
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

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
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

;; TODO We can't disable insert state bindings as some are useful
;; however it would be good to restore 'C-a' and 'C-e'.
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  ;; (setq evil-disable-insert-state-bindings t)
  :config
  (evil-mode))

;; vim-like keybindings everywhere in emacs
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; gl and gL operators, like vim-lion
(use-package evil-lion
  :bind (:map evil-normal-state-map
	      ("gl " . evil-lion-left)
	      ("gL " . evil-lion-right)
	      :map evil-visual-state-map
	      ("gl " . evil-lion-left)
	      ("gL " . evil-lion-right)))

;; gc operator, like vim-commentary
(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode))

;; gx operator, like vim-exchange
;; NOTE using cx like vim-exchange is possible but not as straightforward
(use-package evil-exchange
  :bind (:map evil-normal-state-map
	      ("gx" . evil-exchange)
	      ("gX" . evil-exchange-cancel)))

;; gr operator, like vim's ReplaceWithRegister
(use-package evil-replace-with-register
  :bind (:map evil-normal-state-map
	      ("gr" . evil-replace-with-register)
	      :map evil-visual-state-map
	      ("gr" . evil-replace-with-register)))

;; * operator in visual mode
(use-package evil-visualstar
  :bind (:map evil-visual-state-map
	      ("*" . evil-visualstar/begin-search-forward)
	      ("#" . evil-visualstar/begin-search-backward)))

;; ex commands, which a vim user is likely to be familiar with
(use-package evil-expat)

;; visual hints while editing
(use-package evil-goggles
  :config
  (evil-goggles-use-diff-faces)
  (evil-goggles-mode))

;; ;; like vim-surround
;; (use-package evil-surround
;;   :ensure t
;;   :commands
;;   (evil-surround-edit
;;    evil-Surround-edit
;;    evil-surround-region
;;    evil-Surround-region)
;;   :init
;;   (evil-define-key 'operator global-map "s" 'evil-surround-edit)
;;   (evil-define-key 'operator global-map "S" 'evil-Surround-edit)
;;   (evil-define-key 'visual global-map "S" 'evil-surround-region)
;;   (evil-define-key 'visual global-map "gS" 'evil-Surround-region))

(use-package general
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer lgreen/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  (lgreen/leader-keys
    "SPC" '(execute-extended-command :which-key "M-x")
    "f c" '((lambda () (interactive) (find-file "~/.emacs.default/README.org")) :wk "Edit emacs config")
    "." '(find-file :which-key "find-file"))

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

  ;; Magit
  (lgreen/leader-keys
    "g" '(:ignore t :wk "magit")
    "g g" '(magit-status :wk "Show status"))

  ;; Quit
  (lgreen/leader-keys
    "q" '(:ignore t :wk "quit")
    "q q" '(save-buffers-kill-terminal :wk "Quit"))

  ;; Toggles
  (lgreen/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t t" '(visual-line-mode :wk "Toggle truncated lines"))
  )

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config
  (setq which-key-sort-uppercase-first nil))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
	 ("C-c M-x" . consult-mode-command)
	 ("C-c h" . consult-history)
	 ("C-c k" . consult-kmacro)
	 ("C-c m" . consult-man)
	 ("C-c i" . consult-info)
	 ([remap Info-search] . consult-info)
	 ;; C-x bindings in `ctl-x-map'
	 ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
	 ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
	 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
	 ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
	 ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
	 ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
	 ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
	 ;; Custom M-# bindings for fast register access
	 ("M-#" . consult-register-load)
	 ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
	 ("C-M-#" . consult-register)
	 ;; Other custom bindings
	 ("M-y" . consult-yank-pop)                ;; orig. yank-pop
	 ;; M-g bindings in `goto-map'
	 ("M-g e" . consult-compile-error)
	 ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
	 ("M-g g" . consult-goto-line)             ;; orig. goto-line
	 ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
	 ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-mark)
	 ("M-g i" . consult-imenu)
	 ("M-g I" . consult-imenu-multi)
	 ;; M-s bindings in `search-map'
	 ("M-s d" . consult-find)                  ;; Alternative: consult-fd
	 ("M-s c" . consult-locate)
	 ("M-s g" . consult-grep)
	 ("M-s G" . consult-git-grep)
	 ("M-s r" . consult-ripgrep)
	 ("M-s l" . consult-line)
	 ("M-s L" . consult-line-multi)
	 ("M-s k" . consult-keep-lines)
	 ("M-s u" . consult-focus-lines)
	 ;; Isearch integration
	 ("M-s e" . consult-isearch-history)
	 :map isearch-mode-map
	 ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
	 ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
	 ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
	 ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
	 ;; Minibuffer history
	 :map minibuffer-local-map
	 ("M-s" . consult-history)                 ;; orig. next-matching-history-element
	 ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
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

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
)

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
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

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package helpful
  :ensure t
  :after general
  :general
  (:keymaps 'lgreen/leader-keys
	    "h" '(:ignore t :wk "Help")
	    "h f" '(helpful-callable :wk "Find function")
	    "h v" '(helpful-variable :wk "Find variable")
	    "h k" '(helpful-key :wk "Find key")
	    "h x" '(helpful-command :wk "Find command")))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; use the powerful 'undo-tree'
;; "What good is a mind if you can't change it"
(use-package undo-tree
  :custom
  (evil-undo-system 'undo-tree)
  :init
  (global-undo-tree-mode))

;; Dired - tell it to use 'gls
(setq ls-lisp-use-insert-directory-program t)
(setq insert-directory-program "gls")

(use-package org-bullets
  :ensure t
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(require 'org-tempo)

(use-package magit
  :ensure t
  :general
  (:keymaps 'lgreen/leader-keys
            "g g" 'magit-status))

(use-package lsp-mode)

(use-package lorem-ipsum)

;; Set transparency of emacs
(defun lgreen/transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(use-package dired-narrow)
(use-package origami)
(use-package company)
