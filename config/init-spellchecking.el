;;; init-spellchecking.el --- -*- lexical-binding: t; -*-

;; TODO Setup personal dictionary and project specific dictionary

;;; Jinx
;; Misspelled it first!
;;
;; This package has an external dependency i.e. the `enchant' library
;; - install it using your package manager
;; Use cases:
;; - Common mistyped and misspelled words: hte (the), noone (none/noon)
(use-package jinx
  :after (general evil-collection)
  :init
  (lgreen/leader-define-key
    "t s" '(global-jinx-mode :wk "Toggle spellcheck")
    "c s" '(:ingore :wk "Check spelling")
    "c s s" 'jinx-correct
    "c s a" 'jinx-correct-all
    "c s n" 'jinx-next
    "c s p" 'jinx-previous
    )
  :hook (emacs-startup . global-jinx-mode)
  :bind
  ([remap ispell-word] . 'jinx-correct)
  ("C-," . 'jinx-correct)
  :general
  (:keymaps 'jinx-repeat-map
	    :states '(normal insert visual)
	    "C-j" 'jinx-next
	    "C-k" 'jinx-previous)
  )

;;; _
(provide 'init-spellchecking)
