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
;; Pump up those wpm numbers
(use-package typit)

;;; GPG
;; NOTE: Not sure if we need `pinentry' or just this config
(use-package epg
  :ensure nil
  :custom (epg-pinentry-mode 'loopback))

;;; Pinentry
;; Enter GPG passphrase via Emacs
(use-package pinentry
  :config (pinentry-start))

;;; Direnv
(use-package direnv
  :config (direnv-mode))

;;; EditorConfig
(use-package editorconfig
  :config (editorconfig-mode 1))

;;; Keychain-Environment
;; Re-use environment variables: `SSH_AUTH_SOCK' `SSH_AGENT_PID' `GPG_AGENT'
(use-package keychain-environment
  :ensure (:fetcher github :repo "LambertGreen/keychain-environment")
  :config (keychain-refresh-environment))

;;; World-Clock
(use-package emacs
  :ensure nil
  :custom
  (world-clock-time-format "%a %d %b %l:%M %p %Z")
  (world-clock-list '(("America/Los_Angeles" "Seattle")
		      ("UTC" "UTC")
		      ("Europe/Madrid" "Madrid")
		      ("Africa/Nairobi" "Cape Town")
		      ("Asia/Calcutta" "New Delhi")
		      ("Asia/Tokyo" "Tokyo"))))

;;; _
(provide 'init-utils)
