;; init-themes.el --- -*- lexical-binding: t; -*-

;;; Catppuccin theme
(use-package catppuccin-theme
  :custom
  ;; Options: 'mocha, 'frappe, 'latte, 'macchiato
  (catppuccin-flavor 'mocha))

;;; Doom Themes
(use-package doom-themes
  :demand t
  :custom (doom-one-padded-modeline t))

;;; EF Themes
(use-package ef-themes)

;;; Kaolin Themes
(use-package kaolin-themes)

;;; Spacemacs Theme
(use-package spacemacs-theme)

;;; Chocolate Theme
(use-package chocolate-theme)

;;; Sync theme with system appearance
(use-package theme-settings
  :ensure nil
  :after doom-themes
  :init
  (defvar lgreen/preferred-light-theme 'doom-one-light
    "Theme to use in light mode.")
  (defvar lgreen/preferred-dark-theme 'doom-one
    "Theme to use in dark mode.")

  (defun lgreen/apply-theme-based-on-appearance (appearance)
    "Load theme based on APPEARANCE which is either 'light or 'dark.
If APPEARANCE is not passed, query frame parameters."
    (let ((effective-appearance
           (or appearance
               (frame-parameter nil 'background-mode)))) ;; fallback if nil
      (mapc #'disable-theme custom-enabled-themes)
      (pcase effective-appearance
        ('light (load-theme lgreen/preferred-light-theme t))
        ('dark (load-theme lgreen/preferred-dark-theme t)))))

  (defun lgreen/set-theme-based-on-os ()
    "Set theme based on the operating system."
    (cond
     ;; macOS (emacs-plus or emacs-mac)
     ((memq window-system '(mac ns))
      ;; Attach the hook
      (add-hook 'ns-system-appearance-change-functions #'lgreen/apply-theme-based-on-appearance)
      ;; Set initial theme based on current appearance
      (lgreen/apply-theme-based-on-appearance (frame-parameter nil 'background-mode)))
     ;; Windows and Linux fallback
     ((or (eq system-type 'windows-nt)
          (eq system-type 'gnu/linux))
      (load-theme lgreen/preferred-dark-theme t))))

  ;; Execute immediately
  (lgreen/set-theme-based-on-os))

;;; _
(provide 'init-themes)
