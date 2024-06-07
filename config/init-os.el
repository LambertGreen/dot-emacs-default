;;; init-os.el --- -*- lexical-binding: t; -*-

;; macOS specific config
(use-package emacs
  :ensure nil
  :if (memq window-system '(mac ns))
  :config
  (setq insert-directory-program "gls"
	dired-use-ls-dired t)

  ;; Set Homebrew prefix
  (defvar homebrew-prefix)
  (if (file-directory-p "/opt/homebrew/")
      (setq homebrew-prefix "/opt/homebrew/")
    (setq homebrew-prefix "/usr/local/"))

  ;; Add Homebrew Emacs site-lisp to load-path
  (let ((default-directory (concat homebrew-prefix "share/emacs/site-lisp")))
    (normal-top-level-add-subdirs-to-load-path)))

;;; Ns-Auto-Titlebar
;; On macOS make the titlebar look better
;; The package essentially handles the below config:
;; (set-frame-parameter nil 'ns-appearance 'dark/light)
;; (set-frame-parameter nil 'ns-transparent-titlebar t)
(use-package ns-auto-titlebar
  :when (and (eq system-type 'darwin)
             (or (display-graphic-p) (daemonp)))
  :config (ns-auto-titlebar-mode))

(use-package emacs
  :ensure nil
  :after info
  :if (memq window-system '(mac ns))
  :config
  ;; Add Homebrew Info to Info path
  (add-to-list `Info-directory-list (concat homebrew-prefix "share/info/")))

(use-package emacs
  :ensure nil
  :config
  ;; Enable mouse support in terminal
  (unless window-system
    (require 'mouse)
    (xterm-mouse-mode t)
    (global-set-key [mouse-4] (lambda ()
				(interactive)
				(scroll-down 1)))
    (global-set-key [mouse-5] (lambda ()
				(interactive)
				(scroll-up 1)))
    (defun track-mouse (_))
    (setq mouse-sel-mode t)
    )
  )
;;; _
(provide 'init-os)
