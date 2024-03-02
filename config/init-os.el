;;; init-os.el --- -*- lexical-binding: t; -*-

;; macOS specific config
(use-package emacs
  :ensure nil
  :if (memq window-system '(mac ns))
  :config
  (setq insert-directory-program "gls"
	dired-use-ls-dired t))

;;; _
(provide 'init-os)
