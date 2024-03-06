;;; init-prog.el --- -*- lexical-binding: t; -*-

(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
	 ("\\.cmake\\'"         . cmake-mode)))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(use-package ssh-config-mode
  :ensure t
  :mode ((".ssh/config\\'"       . ssh-config-mode)
         ("sshd?_config\\'"      . ssh-config-mode)
         ("known_hosts\\'"       . ssh-known-hosts-mode)
         ("authorized_keys\\'"   . ssh-authorized-keys-mode)))

(use-package yaml-mode
  :ensure t)

;;; _
(provide 'init-prog)
