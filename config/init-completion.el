;;; init-completion.el --- -*- lexical-binding: t; -*-

(use-package company
  :ensure t
  :init
  (setq company-idle-delay 0.1  ; Show suggestions after a small delay
	company-minimum-prefix-length 2 ; Start completing after 2 characters
	company-show-numbers t) ; Show numbers for easy selection
  :config
  (global-company-mode t)) ; Enable Company mode globally

;;; _
(provide 'init-completion)
