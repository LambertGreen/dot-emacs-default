;; init-spellchecking.el --- -*- lexical-binding: t; -*-


;; TODO Setup personal dictionary and project specific dictionary

;;; Jinx
;; Misspelled it first!
;;
;; This package has an external dependency i.e. the `enchant' library
;; - install it using your package manager
;; Use cases:
;; - Common mistyped and misspelled words: hte (the), noone (none/noon)
(use-package jinx
  :if (not (eq system-type 'windows-nt))
  :hook ((emacs-startup . global-jinx-mode)
         ((c++-mode c++-ts-mode) . lgreen/jinx-setup-for-c++-modes))
;;;; Keymaps
  :bind
  ([remap ispell-word] . 'jinx-correct)
  ("C-," . 'jinx-correct)
  :init
  (lgreen/leader-define-key
    "t s" '(global-jinx-mode :wk "toggle spellcheck")
    "x s" '(:ignore t :wk "Spelling")
    "x s s" '(jinx-correct :wk "correct")
    "x s a" '(jinx-correct-all :wk "correct all")
    "x s n" '(jinx-next :wk "correct-next")
    "x s p" '(jinx-previous :wk "correct-previous"))
  :config
;;;; Functions
  (defun lgreen/jinx-skip-c++-includes-for-system-headers (start)
    "Skip checking #include of system libraries."
    (save-excursion
      (goto-char start)
      (beginning-of-line)
      (looking-at "^#include <.*>$")))
  (defun lgreen/jinx-setup-for-c++-modes ()
    "Set up Jinx predicates and camelCase handling for C++ modes."
    (add-to-list 'jinx--predicates #'lgreen/jinx-skip-c++-includes-for-system-headers)
    (add-to-list 'jinx-camel-modes 'c++-mode)
    (add-to-list 'jinx-camel-modes 'c++-ts-mode)))

;;; _
(provide 'init-spellchecking)
