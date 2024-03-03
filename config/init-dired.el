;;; init-dired.el --- -*- lexical-binding: t; -*-

;; Fancy icons for Dired
(use-package nerd-icons-dired
  :ensure t
  :after (:all nerd-icons dired)
  :hook (dired-mode . nerd-icons-dired-mode))

;; Colorful dired
(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

;; `find-dired' alternative using `fd'
(when (executable-find "fd")
  (use-package fd-dired
    :ensure t))

;;; _
(provide 'init-dired)
