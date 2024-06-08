;;; init-themes.el --- -*- lexical-binding: t; -*-

;;; Catppuccin theme
(use-package catppuccin-theme
  :custom
  ;; Options: 'mocha, 'frappe, 'latte, 'macchiato
  (catppuccin-flavor 'mocha))

;;; Doom Themes
(use-package doom-themes)

;;; EF Themes
(use-package ef-themes)

;;; Kaolin Themes
(use-package kaolin-themes)

;;; Spacemacs Theme
(use-package spacemacs-theme)

;;; Chocolate Theme
(use-package chocolate-theme)

;;; Sync theme with system appearance
(use-package emacs
  :ensure nil
  :init
  (defun lgreen/sync-theme-with-system-appearance (appearance)
    "Load theme taking current system APPEARANCE into account."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (load-theme 'doom-one-light t))
      ('dark (load-theme 'doom-one t))))
  (add-hook 'ns-system-appearance-change-functions  #'lgreen/sync-theme-with-system-appearance))

;;; _
(provide 'init-themes)
