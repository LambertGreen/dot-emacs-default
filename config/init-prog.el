;;; init-prog.el --- -*- lexical-binding: t; -*-

;;; Treesit-Auto
;; Get all the langs
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  (treesit-font-lock-level 4)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; Cmake
;; Let's make them cpp projects
(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
	 ("\\.cmake\\'"         . cmake-mode)))

;;; Dockerfile
;; Contain your excitement
(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

;;; SSH config
;; Silently connecting the dots
(use-package ssh-config-mode
  :ensure t
  :mode ((".ssh/config\\'"       . ssh-config-mode)
         ("sshd?_config\\'"      . ssh-config-mode)
         ("known_hosts\\'"       . ssh-known-hosts-mode)
         ("authorized_keys\\'"   . ssh-authorized-keys-mode)))

;;; Yaml
;; Configs everywhere
(use-package yaml-mode
  :ensure t)

;;; Markdown
;; Baby org
(use-package markdown-mode
  :ensure t)

;;; _
(provide 'init-prog)
