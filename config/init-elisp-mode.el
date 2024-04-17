;;; init-elisp.el --- -*- lexical-binding: t; -*-

(use-package emacs-lisp-mode
  :ensure nil ; since emacs-lisp-mode is built-in
  :init
  (lgreen/local-leader-define-key
    :states 'normal
    :keymaps 'emacs-lisp-mode-map
    "e" '(:ignore t :wk "eval")
    "e b" 'eval-buffer
    "e d" 'eval-defun
    "e e" 'eval-last-sexp
    "e r" 'eval-region)
  :hook
  (emacs-lisp-mode . hs-minor-mode))

;;; _
(provide 'init-elisp-mode)
