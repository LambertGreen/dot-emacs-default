;;; init-dashboard.el --- -*- lexical-binding: t; -*-

(use-package dashboard
  :ensure t
  :hook
  ((elpaca-after-init . dashboard-insert-startupify-lists)
   (elpaca-after-init . dashboard-initialize))
  :config
  (dashboard-setup-startup-hook))

;;; _
(provide 'init-dashboard)
