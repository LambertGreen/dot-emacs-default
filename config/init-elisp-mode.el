;; init-elisp.el --- -*- lexical-binding: t; -*-


;;; Emacs-Lisp-Mode
;; brace yourself!
;; TODO configure typing quote symbol to not create a pair in elisp-mode
(use-package emacs-lisp-mode
  :ensure nil ; since emacs-lisp-mode is built-in
  :init
  (lgreen/local-leader-define-key
    :states 'normal
    :keymaps 'emacs-lisp-mode-map
    "e" '(:ignore t :wk "Eval")
    "e b" 'eval-buffer
    "e d" 'eval-defun
    "e e" 'eval-last-sexp
    "e r" 'eval-region)
  (defun lgreen/imenu-elisp-sections ()
    (add-to-list 'imenu-generic-expression '("Package" "^\\s-*(use-package\\s-+\\(\\_<.*?\\_>\\)" 1)))
  :hook ((emacs-lisp-mode . hs-minor-mode)
         (emacs-lisp-mode . lgreen/imenu-elisp-sections)))

;;; Eros
;; Evaluation Result Overlays for Emacs Lisp
(use-package eros
  :config (eros-mode 1))

;; _
(provide 'init-elisp-mode)

;; Local Variables:
;; jinx-local-words: "elisp emacs eval"
;; End:
