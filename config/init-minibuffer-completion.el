;;; init-minibuffer-completion.el --- -*- lexical-binding: t; -*-


;;; Vertico
;; Vertical completion UI
(use-package vertico
  :custom
  ;; Different scroll margin
  (vertico-scroll-margin 0)
  (vertico-count 20)
  (vertico-cycle t)
  :init
  (vertico-mode)
  :hook (minibuffer-setup . vertico-repeat-save)
  :config
  (lgreen/leader-define-key
    "'" '(vertico-repeat :wk "Repeat Search")))

;;; Marginalia
;; Annotations for minibuffer completions
(use-package marginalia
  :bind (:map minibuffer-local-map
	      ("M-a" . marginalia-cycle))
  :init (marginalia-mode))

;;; Consult
;; Making buffer completions nicer
(use-package consult
  :after general
  :bind
  ([remap next-matching-history-element] . consult-history)
  ([remap previous-matching-history-element] . consult-history)
  :init
  (lgreen/leader-define-key
    "*" '(lgreen/ripgrep-symbol-at-point :wk "Symbol search")
    "b b" '(consult-buffer :wk "Switch buffer")
    "h t" '(consult-theme :wk "Switch theme")
    "s b" '(consult-line :wk "Search buffer")
    "s p" '(consult-ripgrep :wk "Search project files")
    "s i" '(consult-imenu :wk "Jump to symbol")
    "s d" '(consult-locate :wk "Search current directory"))

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

  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
	(lambda (&rest args)
	  (apply (if vertico-mode
		     #'consult-completion-in-region
		   #'completion--in-region)
		 args)))

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.

  (defun lgreen/ripgrep-symbol-at-point ()
    "Performs a search in the current buffer for thing at point."
    (interactive)
    (consult-ripgrep nil (thing-at-point 'symbol)))

  :hook (completion-list-mode . consult-preview-at-point-mode))

;;; Consult-Todo
;; NOTE The directory and project based searches are done using a hard coded grep-command
;; TODO Fork Consult-Todo and add ripgrep support.
;; We can use `magit-todos` for project todos but I prefer the minibuffer interface (item counts, filtering, exporting)
(use-package consult-todo
  :after (general consult hl-todo)
  :init
  (lgreen/leader-define-key
    "s t" '(consult-todo :wk "Search todos")
    "s T" '(consult-todo-all :wk "Search all todos")))

;;; Fussy
;; Flex matching
(use-package fussy
  :config (push 'fussy completion-styles))

;;; _
(provide 'init-minibuffer-completion)
