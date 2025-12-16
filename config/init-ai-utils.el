;; init-ai-utils.el --- -*- lexical-binding: t; -*-

;;; Gptel
;; I just called to... ask you
(use-package gptel)

;;; Claude-Code-Ide
;; Hey Jarvis... I mean Claude
(use-package claude-code-ide
  :ensure (:fetcher github :repo "manzaltu/claude-code-ide.el")
  :custom (claude-code-ide-terminal-backend 'eat)
  :bind ("C-c a" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup))

;;
(provide 'init-ai-utils)
