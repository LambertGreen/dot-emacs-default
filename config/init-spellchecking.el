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
  :after general
  :init
  (lgreen/leader-define-key
    "t s" '(global-jinx-mode :wk "Toggle spellcheck")
    "x s" '(:ingore t :wk "spelling")
    "x s s" '(jinx-correct :wk "correct")
    "x s a" '(jinx-correct-all :wk "correct all")
    "x s n" '(jinx-next :wk "correct-next")
    "x s p" '(jinx-previous :wk "correct-previous"))
  :hook (emacs-startup . global-jinx-mode)
  :bind
  ([remap ispell-word] . 'jinx-correct)
  ("C-," . 'jinx-correct))

;; _
(provide 'init-spellchecking)
