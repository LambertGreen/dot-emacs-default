;;; init-minibuffer.el --- -*- lexical-binding: t; -*-


;;; Minibuffer
(use-package minibuffer
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

  (defun my/minibuffer-keybindings ()
    "Set custom keybindings for the minibuffer."
    (define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)  ; Modify or kill the word backward
    (define-key minibuffer-local-map (kbd "C-u") 'backward-kill-sentence)) ; Kill text back to the beginning
  :hook
  ((minibuffer-setup-hook . my/minibuffer-keybindings)
   (minibuffer-setup-hook . cursor-intangible-mode))
  :custom
  ;; Enable recursive minibuffers
  (enable-recursive-minibuffers t)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; TODO Evaluate if you want to keep this setting
  (setq read-extended-command-predicate
	#'command-completion-default-include-p))

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
  (lgreen/leader-keys
    "'" '(vertico-repeat :wk "Repeat Search")))

;;; Marginalia
;; Annotaions for minibuffer completions
(use-package marginalia
  :init (marginalia-mode))

;;; _
(provide 'init-minibuffer)
