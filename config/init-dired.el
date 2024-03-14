;;; init-dired.el --- -*- lexical-binding: t; -*-

;;; Nerd-Icons-Dired
;; Fancy icons for Dired
(use-package nerd-icons-dired
  :ensure t
  :after (:all nerd-icons dired)
  :hook (dired-mode . nerd-icons-dired-mode))

;;; Diredfl
;; Colorful dired
(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

;;; Fd-Dired
;; `find-dired' alternative using `fd'
(use-package fd-dired
  :ensure t
  :if (executable-find "fd"))

;;; _
(provide 'init-dired)
