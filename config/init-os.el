;;; init-os.el --- -*- lexical-binding: t; -*-

;; macOS specific config
(use-package emacs
  :ensure nil
  :if (memq window-system '(mac ns))
  :config
  (setq insert-directory-program "gls"
	dired-use-ls-dired t)

  (defvar homebrew-prefix)
  (if (file-directory-p "/opt/homebrew/")
      (setq homebrew-prefix "/opt/homebrew/")
    (setq homebrew-prefix "/usr/local/"))

  ;; Add Homebrew Emacs site-lisp to load-path
  (let ((default-directory (concat homebrew-prefix "share/emacs/site-lisp")))
    (normal-top-level-add-subdirs-to-load-path)))

(use-package emacs
  :ensure nil
  :after info
  :if (memq window-system '(mac ns))
  :config
  ;; Add Homebrew Info to Info path
  (add-to-list `Info-directory-list (concat homebrew-prefix "share/info/")))
;;; _
(provide 'init-os)
