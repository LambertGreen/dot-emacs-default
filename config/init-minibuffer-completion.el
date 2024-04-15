;;; init-minibuffer-completion.el --- -*- lexical-binding: t; -*-


;;; Vertico
;; Veritcal completion UI
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
;; Annotaions for minibuffer completions
(use-package marginalia
  :init (marginalia-mode))

;;; Consult
;; Making buffer completions nicer
(use-package consult
  :after general
  :init
  (lgreen/leader-define-key
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
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode))

;;; Conslut-Todo
;; TODO Consider removing in favor of `magit-todos`
(use-package consult-todo
  :after (general consult hl-todo)
  :init
  (lgreen/leader-define-key
    "s t" '(consult-todo :wk "Search todos")
    "s T" '(consult-todo-all :wk "Search all todos")))

;;; _
(provide 'init-minibuffer-completion)
