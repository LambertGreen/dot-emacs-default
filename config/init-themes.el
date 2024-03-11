;;; init-themes.el --- -*- lexical-binding: t; -*-

;; Get a beautiful and functional theme
(use-package catppuccin-theme
  :ensure t
  :custom
  ;; Options: 'mocha, 'frappe, 'latte, 'macchiato
  (catppuccin-flavor 'mocha))

(use-package kanagawa-theme
  :ensure t)

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-tokyo-night :no-confirm))

(use-package ef-themes
  :ensure t)

;;; _
(provide 'init-themes)
