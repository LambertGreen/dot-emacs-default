;; init-elisp.el --- -*- lexical-binding: t; -*-

;;; Emacs-Lisp-Mode
;; brace yourself!
(use-package emacs-lisp-mode
  :ensure nil
  :hook (((emacs-lisp-mode lisp-interaction-mode) . hs-minor-mode)
         (emacs-lisp-mode . lgreen/imenu-elisp-sections))
  :init
;;;; Keymaps
;;;;; Eval
  (dolist (mode '(emacs-lisp-mode-map lisp-interaction-mode-map))
    (lgreen/local-leader-define-key
      :keymaps mode
      "e" '(:ignore t :wk "Eval")
      "e b" 'eval-buffer
      "e d" 'eval-defun
      "e e" 'eval-last-sexp
      "e r" 'eval-region))
;;;; Functions
  (defun lgreen/imenu-elisp-sections ()
    "Extend imenu to include `use-package` sections in Emacs Lisp."
    (add-to-list 'imenu-generic-expression
                 '("Package" "^\\s-*(use-package\\s-+\\(\\_<.*?\\_>\\)" 1) t)))

;;; Eros
;; Evaluation Result Overlays for Emacs Lisp
(use-package eros
  :hook ((emacs-lisp-mode lisp-interaction-mode) . eros-mode)
  :config (eros-mode 1))

;;; _
(provide 'init-elisp-mode)

;; Local Variables:
;; jinx-local-words: "elisp emacs eval"
;; End:
