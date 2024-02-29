;;; init-elisp.el --- -*- lexical-binding: t; -*-

(use-package emacs-lisp-mode
  :ensure nil ; since emacs-lisp-mode is built-in
  :hook (emacs-lisp-mode . hs-minor-mode)
  )

;;; _
(provide 'init-elisp-mode)
