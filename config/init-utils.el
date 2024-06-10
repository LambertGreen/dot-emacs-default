;;; init-utils.el --- -*- lexical-binding: t; -*-

;;; Tldr
;; When there is no time for man
(use-package tldr)

;;; Devdocs
;; RTFM
(use-package devdocs)

;;; Speed-Type
;; pump up those wpm numbers
(use-package speed-type)

;;; Typit
;; pump up those wpm numbers
(use-package typit)

;;; Pinentry
(use-package pinentry
  :config (pinentry-start))

;;; Direnv
(use-package direnv)

;;; EditorConfig
(use-package editorconfig)

;;; Keychain-Environment
(use-package keychain-environment
  :ensure (:fetcher github :repo "LambertGreen/keychain-environment")
  :config (keychain-refresh-environment))

;;; _
(provide 'init-utils)
