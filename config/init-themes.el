;; init-themes.el --- -*- lexical-binding: t; -*-


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
  :after doom-themes
  :if (memq window-system '(mac ns))
  :init
  (defun lgreen/sync-theme-with-system-appearance (appearance)
    "Load theme taking current system APPEARANCE into account."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (load-theme 'doom-one-light t))
      ('dark (load-theme 'doom-one t))))
  (add-hook 'ns-system-appearance-change-functions  #'lgreen/sync-theme-with-system-appearance)
  :config
  ;; Determine current appearance and set the appropriate theme at startup
  (lgreen/sync-theme-with-system-appearance 'ns-system-appearance))

;; _
(provide 'init-themes)

;; Local Variables:
;; jinx-local-words: "macchiato"
;; End:
