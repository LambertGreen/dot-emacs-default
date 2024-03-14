;;; init-themes.el --- -*- lexical-binding: t; -*-

;;; Catppuccin theme
(use-package catppuccin-theme
  :ensure t
  :custom
  ;; Options: 'mocha, 'frappe, 'latte, 'macchiato
  (catppuccin-flavor 'mocha))

;;; Kanagawa theme
(use-package kanagawa-theme
  :ensure t)

;;; Doom Themes
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-tokyo-night :no-confirm))

;;; EF Themes
(use-package ef-themes
  :ensure t)

;;; Kaolin Themes
(use-package kaolin-themes
  :ensure t)

;;; Spacemacs Theme
(use-package spacemacs-theme
  :ensure t)

;;; Chocolate Theme
(use-package chocolate-theme
  :ensure t)

;;; _
(provide 'init-themes)
