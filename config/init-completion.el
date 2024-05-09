;;; init-completion.el --- -*- lexical-binding: t; -*-


;; FIXME Fix auto-completion issue where fast typing can result in mistaken acceptence of incorrect completion

;;; Dabbrev
;; short and sweet
(use-package dabbrev
  :ensure nil
  ;; TODO Need to find another binding since we are taking "C-;" for flyspell
  ;; :general
  ;; (evil-insert-state-map
  ;;  "C-;" 'dabbrev-expand)
  )

;;; Company
;; not a crowd
(use-package company
  :init
  (setq company-idle-delay 0.1  ; Show suggestions after a small delay
	company-minimum-prefix-length 2 ; Start completing after 2 characters
	company-show-numbers t) ; Show numbers for easy selection
  :config
  (global-company-mode t)) ; Enable Company mode globally

;;; _
(provide 'init-completion)
