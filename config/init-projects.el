;;; init-projects.el --- -*- lexical-binding: t; -*-

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (setq projectile-project-search-path '(("~/dev" . 7))))

;;; _
(provide 'init-projects)
