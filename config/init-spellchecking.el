;;; init-spellchecking.el --- -*- lexical-binding: t; -*-

;; TODO Setup personal dictionary and project specific dictionary

;;; Flyspell
;; High flying spelling bees
;; Use cases:
;; - Common mistyped and misspelled words: hte (the), noone (none/noon)
(use-package flyspell
  :ensure nil
  :after general
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  :general
  (:states 'normal
	   "z =" 'ispell-word
	   "z g" 'flyspell-buffer)
  :config
  (lgreen/leader-define-key
    "t s" '(lgreen/toggle-flyspell-mode :wk "Toggle spellcheck"))

  (defun lgreen/toggle-flyspell-mode ()
    "Toggle `flyspell-mode', respecting major mode.

This will toggle `flyspell-prog-mode' in `prog-mode' buffers, and
`flyspell-mode' everywhere else."
    (interactive)
    (if (and (derived-mode-p 'prog-mode)
	     (not flyspell-mode))
	(flyspell-prog-mode)
      (flyspell-mode 'toggle))))

;;; Flyspell-Correct
;; Fuzzy select the correction
;; - Quote: Can we fix it? Yes, we can!
(use-package flyspell-correct
  :after flyspell)

;;; Consult-Flyspell
;; Fuzzy select the target
(use-package consult-flyspell
  :ensure (:fetcher gitlab :repo "Olson/consult-flyspell")
  :after (flyspell flyspell-correct)
  :init
  ;; NOTE I had issues setting this up the usual way i.e. using `:custom` and
  ;; not using `require` directly in the hook.
  (setq consult-flyspell-select-function 'flyspell-correct-at-point)
  (setq consult-flyspell-set-point-after-word t)
  (setq consult-flyspell-always-check-buffer t)
  (general-def
    :keymaps 'flyspell-mode-map
    "C-;" 'consult-flyspell)
  :hook (flyspell-mode . (lambda () (require 'consult-flyspell))))

;;; _
(provide 'init-spellchecking)
