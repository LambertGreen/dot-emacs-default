;; init-minibuffer.el --- -*- lexical-binding: t; -*-


;;; Minibuffer
(use-package minibuffer
  :ensure nil
  :init
  (general-def :keymaps 'minibuffer-local-map
    :states '(normal insert)
    "C-n" 'next-line-or-history-element
    "C-p" 'previous-line-or-history-element
    "C-;" 'embark-act)

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
  :hook
  (minibuffer-setup-hook . cursor-intangible-mode)
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

;; _
(provide 'init-minibuffer)

;; Local Variables:
;; jinx-local-words: "Vertico minibuffer minibuffers"
;; End:
