;;; init-elisp.el --- -*- lexical-binding: t; -*-

(use-package emacs-lisp-mode
  :ensure nil ; since emacs-lisp-mode is built-in
  :init
  (lgreen/local-leader-keys
    :states 'normal
    :keymaps 'emacs-lisp-mode-map
    "e" '(:ignore t :wk "eval")
    "e b" 'eval-buffer
    "e d" 'eval-defun
    "e e" 'eval-last-sexp
    "e r" 'eval-region)
  (defun lgreen/modify-syntax-entry-for-lisp()
    "Adjust syntax table for lisp programming modes."
    (modify-syntax-entry ?- "w")
    (modify-syntax-entry ?/ "w"))
  :hook
  (emacs-lisp-mode . hs-minor-mode)
  (emacs-lisp-mode . lgreen/modify-syntax-entry-for-lisp))

;;; _
(provide 'init-elisp-mode)
