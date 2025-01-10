;; init-utils.el --- -*- lexical-binding: t; -*-

;;; Tldr
;; When there is no time for man
(use-package tldr)

;;; Devdocs
;; RTFM
(use-package devdocs)

;;; Speed-Type
;; Pump up those wpm numbers
(use-package speed-type)

;;; Typit
;; Pump up those wpm numbers
(use-package typit)

;;; Keychain-Environment
;; Re-use environment variables: `SSH_AUTH_SOCK' `SSH_AGENT_PID' `GPG_AGENT'
(use-package keychain-environment
  :ensure (:fetcher github :repo "LambertGreen/keychain-environment")
  :hook (after-init . keychain-refresh-environment))

;;; GPG
;; NOTE: Not sure if we need `pinentry' or just this config
(use-package epg
  :ensure nil
  :custom (epg-pinentry-mode 'loopback))

;;; Pinentry
;; Enter GPG passphrase via Emacs
(use-package pinentry
  :after (epg keychain-environment)
  :unless (eq system-type 'windows-nt)
  :hook (after-init . pinentry-start))

;;; Direnv
(use-package direnv
  :unless (eq system-type 'windows-nt)
  :hook (project-find-functions . (lambda (_)
                                    (direnv-enable-mode))))

;;; EditorConfig
;; Follow the conventions of the repo
(use-package editorconfig
  :hook (project-find-functions . (lambda (_)
                                    (editorconfig-mode 1)))
  :config
  (add-to-list 'editorconfig-exclude-modes 'org-mode))

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

;;; GNU Plot
(use-package gnuplot)

;;; Proced
;; Reach for this before htop/btop/etc.
(use-package proced
  :ensure nil
  :custom (proced-enable-color-flag t))

;;; PDF-Tools
(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install))

;;; _
(provide 'init-utils)

;; Local Variables:
;; jinx-local-words: "LambertGreen keychain utils"
;; End:
