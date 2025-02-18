;; init-editor.el --- -*- lexical-binding: t; -*-

;;; Persistent Scratch
(use-package persistent-scratch
  :init
  (persistent-scratch-setup-default))

;;; Which-Key
;; Which key? This one.
;; NOTE Press "?" to disambiguate long entries with the same prefix
(use-package which-key
  :after general
  :custom
  ;; Going with empty ellipsis to avoid alignment issues,
  ;; which is fine given the overlong items are clearly
  ;; up against the limit
  ;; (which-key-ellipsis "") ;; options: "︙",
  ;; An alternate solution to the alignment issue
  (which-key-don't-use-unicode nil)
  (which-key-separator " → ")
  ;; (which-key-sort-order #'which-key-key-order-alpha)
  ;; (which-key-sort-uppercase-first nil)
  ;; (which-key-add-column-padding 1)
  ;; (which-key-max-display-columns nil)
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

;;;; Keymaps
  (general-def embark-command-map
    "y" 'embark-copy-as-kill)

  (lgreen/leader-define-key
    "a a" '(embark-dwim :wk "act [dwim]")
    "a A" '(embark-act :wk "act")
    "h B" '(embark-bindings :wk "explore bindings"))

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
  :after (general evil)
  :commands (helpful-at-point
             helpful-function
             helpful-variable
             helpful-key
             helpful-command
             helpful-callable
             helpful-symbol)
  :init
  (setq evil-lookup-func #'helpful-at-point)
;;;; Keymaps
  (lgreen/leader-define-key
    "h f" '(helpful-function :wk "describe function")
    "h v" '(helpful-variable :wk "describe variable")
    "h k" '(helpful-key :wk "describe key")
    "h c" '(helpful-command :wk "describe command")
    "h C" '(helpful-callable :wk "describe callable")
    "h s" '(helpful-symbol :wk "describe symbol")
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
;; Bringing balance to the force
(use-package smartparens
  :commands (smartparens-global-mode sp-local-pair)
  :init
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  ;; Disable auto-pairing of single quotes in emacs-lisp-mode
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil))

;;; Ripgrep in Emacs
;; -- "What was lost is now found"
(use-package rg
  :commands (rg-enable-default-bindings)
  :init (rg-enable-default-bindings))

;;; Dumb-Jump
;; Give it your best shot
;; TODO Are you using dump-jump or not?
(use-package dumb-jump)

;;; _
(provide 'init-editor)