;;; init-prog-modes.el --- -*- lexical-binding: t; -*-

;;; Treesit-Auto
;; Get all the langs
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  (treesit-font-lock-level 4)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; Cmake
;; Let's make them cpp projects
(use-package cmake-mode
  :mode
  (("CMakeLists\\.txt\\'" . cmake-mode)
   ("\\.cmake\\'"         . cmake-mode)))

;;; Dockerfile
;; Contain your excitement
(use-package dockerfile-mode
  :mode "Dockerfile\\'")

;;; SSH config
;; Silently connecting the dots
(use-package ssh-config-mode
  :mode
  ((".ssh/config\\'"       . ssh-config-mode)
   ("sshd?_config\\'"      . ssh-config-mode)
   ("known_hosts\\'"       . ssh-known-hosts-mode)
   ("authorized_keys\\'"   . ssh-authorized-keys-mode)))

;;; Yaml
;; Configs everywhere
(use-package yaml-mode)

;;; Markdown
;; Baby org
(use-package markdown-mode)

;;; Groovy
;; Get into the groove boy
(use-package groovy-mode)

;;; _
(provide 'init-prog-modes)
