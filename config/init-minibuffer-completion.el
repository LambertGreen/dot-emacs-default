;; init-minibuffer-completion.el --- -*- lexical-binding: t; -*-


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
    "'" '(vertico-repeat :wk "repeat search")))

;;; Marginalia
;; Annotations for minibuffer completions
(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-a" . marginalia-cycle))
  :init (marginalia-mode))

;;; Consult
;; Making minibuffer completions nicer
;;
(use-package consult
  :after general
  :bind
  ([remap next-matching-history-element] . consult-history)
  ([remap previous-matching-history-element] . consult-history)
  :init
  (lgreen/leader-define-key
    ;; Symbol-at-point
    "*" '(lgreen/ripgrep-symbol-at-point :wk "symbol search")

    ;; File
    "f d" '(lgreen/consult-fd-default-dir-with-args :wk "find file with [fd]")
    "f l" '(consult-locate :wk "find file with [locate]")

    ;; Search
    "s b" '(consult-line :wk "Search buffer")
    "s s" '(lgreen/consult-ripgrep-with-args-in-default-directory :wk "search with [ripgrep]")
    "s i" '(consult-imenu :wk "jump to symbol")
    "s I" '(consult-imenu-multi :wk "jump to symbol (multi-file)")
    "s o" '(consult-outline :wk "jump to heading")

    ;; Buffer
    ;; NOTE: narrowing in the minibuffer is supported with the following keys:
    ;; - <SPC>     -> ephemeral buffers
    ;; - <* SPC>   -> modified buffers
    ;; - <b SPC>   -> only buffers
    ;; - <f SPC>   -> Files
    "b b" '(consult-buffer :wk "switch buffer")

    ;; Project
    "p d" '(lgreen/consult-fd-with-args :wk "find file with [fd]")
    "p b" '(consult-project-buffer :wk "switch buffer")
    "p s" '(lgreen/consult-ripgrep-with-args :wk "search files with [ripgrep]")

    ;; Theme
    "h t" '(consult-theme :wk "switch theme")
    )

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

  (defun lgreen/consult-fd-with-args (&optional args)
    "Run `consult-fd' with the option to modify arguments when `C-u' is provided.
    If no universal argument is given, it runs with the original `consult-fd-args'."
    (interactive "P")
    (if args
        ;; TODO Open an issue on `consult-fd' to make `consult-fd-args' a string
        (let* ((default-args "fd --full-path --color=never ")
               (input-args (read-string (format "fd args (%s): " default-args) default-args)))
          (let ((consult-fd-args (split-string input-args)))
            (consult-fd)))
      (consult-fd)))

  (defun lgreen/consult-fd-with-args-in-default-directory (&optional args)
    "Run `lgreen/consult-fd-with-args' in the `default-directory', overriding running in `project-directory' if
    in an project context."
    (interactive "P")
    (if args
        ;; TODO Open an issue on `consult-fd' to make `consult-fd-args' a string
        (let* ((default-args "fd --full-path --color=never ")
               (input-args (read-string (format "Fd args (%s): " default-args) default-args)))
          (let ((consult-fd-args (split-string input-args)))
            (consult-fd default-directory)))
      (consult-fd default-directory)))

  (defun lgreen/consult-ripgrep-with-args (&optional args)
    "Run `consult-ripgrep' with the option to modify arguments when `C-u' is provided.
    If no universal argument is given, it runs with the original `consult-ripgrep-args'."
    (interactive "P")
    (if args
        (let* ((default-args consult-ripgrep-args)
               (consult-ripgrep-args (read-string (format "Ripgrep args (%s): " default-args) default-args)))
          (consult-ripgrep))
      (consult-ripgrep)))

  (defun lgreen/consult-ripgrep-with-args-in-default-directory (&optional args)
    "Run `lgreen/consult-ripgrep-with-args' in the `default-directory', overriding running in `project-directory' if
    in an project context."
    (interactive "P")
    (if args
        (let* ((default-args consult-ripgrep-args)
               (consult-ripgrep-args (read-string (format "Ripgrep args (%s): " default-args) default-args)))
          (consult-ripgrep default-directory))
      (consult-ripgrep default-directory)))

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
;; TODO Do we need fussy?
;; Let's see how things go with fussy disabled
(use-package fussy
  :disabled t
  :config (push 'fussy completion-styles))

;; _
(provide 'init-minibuffer-completion)

;; Local Variables:
;; jinx-local-words: "Vertico magit minibuffer ripgrep"
;; End:
