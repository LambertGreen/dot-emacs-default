;;; init-dashboard.el --- -*- lexical-binding: t; -*-

(use-package dashboard
  :custom
  (dashboard-center-content t)
  (dashboard-startup-banner 'logo)
  (dashboard-vertically-center-content t)
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-projects-switch-function 'projectile-persp-switch-project)
  (dashboard-items '((recents   . 5)
		     (projects  . 5)
		     (bookmarks . 5)
		     (registers . 5)))
  :hook
  ((elpaca-after-init . dashboard-insert-startupify-lists)
   (elpaca-after-init . dashboard-initialize))

  :config (dashboard-setup-startup-hook))

;;; _
(provide 'init-dashboard)
