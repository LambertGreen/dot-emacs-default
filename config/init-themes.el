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
(use-package ef-themes
  :ensure (:host github :repo "protesilaos/ef-themes"))

;;; Kaolin Themes
(use-package kaolin-themes)

;;; Spacemacs Theme
(use-package spacemacs-theme)

;;; Chocolate Theme
(use-package chocolate-theme)

;;; Theme configuration
;; Hardcode dark theme on all platforms. Internal shells (EAT, vterm) cannot
;; dynamically switch, so the editor must match the shell default.
;; See docs/THEME_STRATEGY.org for full rationale.
(use-package theme-settings
  :ensure nil
  :after (doom-themes solaire-mode)
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
     ;; Disabled: follow OS appearance. Preferred behavior but causes mismatch
     ;; with internal shells that cannot dynamically switch.
     ;; Uncomment to re-enable if shell theme propagation is solved.
     ;; ((memq window-system '(mac ns))
     ;;  (add-hook 'ns-system-appearance-change-functions #'lgreen/apply-theme-based-on-appearance)
     ;;  (lgreen/apply-theme-based-on-appearance (frame-parameter nil 'background-mode)))

     ;; All platforms: hardcode dark
     ((or (memq window-system '(mac ns))
          (eq system-type 'windows-nt)
          (eq system-type 'gnu/linux))
      (load-theme lgreen/preferred-dark-theme t))))

  ;; Execute immediately
  (lgreen/set-theme-based-on-os))

;;; _
(provide 'init-themes)
