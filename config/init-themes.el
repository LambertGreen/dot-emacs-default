;;; init-themes.el --- -*- lexical-binding: t; -*-

;;; Catppuccin theme
(use-package catppuccin-theme
  :custom
  ;; Options: 'mocha, 'frappe, 'latte, 'macchiato
  (catppuccin-flavor 'mocha))

;;; Kanagawa theme
(use-package kanagawa-theme)

;;; Doom Themes
(use-package doom-themes
  :config
  (load-theme 'doom-tokyo-night :no-confirm))

;;; EF Themes
(use-package ef-themes)

;;; Kaolin Themes
(use-package kaolin-themes)

;;; Spacemacs Theme
(use-package spacemacs-theme)

;;; Chocolate Theme
(use-package chocolate-theme)

;;; _
(provide 'init-themes)
