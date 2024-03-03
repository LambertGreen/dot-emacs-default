;;; init-workspaces.el --- -*- lexical-binding: t; -*-

(use-package perspective
  :ensure t
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  :init
  (persp-mode))

(use-package persp-projectile
  :ensure t)

;;; _
(provide 'init-workspaces)
