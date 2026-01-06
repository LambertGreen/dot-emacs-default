;; init-server.el --- -*- lexical-binding: t; -*-

;;; Server
;; Enable emacsclient connections for external tool integration (e.g., AI assistants)
;;
;; NOTE: Server is disabled by default. Enable manually when needed:
;;   M-x server-start
;;
;; Why not auto-start?
;; - Multiple Emacs instances (org, dotfiles, work projects) would conflict
;; - Instances have different lifetimes (work Emacs may crash from LSP/terminal load)
;; - Cross-platform usage (macOS GUI, Linux desktop, SSH) complicates server management

(use-package server
  :ensure nil
  :disabled t  ; Remove this line to enable auto-start
  :defer 1
  :config
  (unless (server-running-p)
    (server-start)))

;;; _
(provide 'init-server)
