;;; init-file-management.el --- -*- lexical-binding: t; -*-

;;; Nerd-Icons-Dired
;; Fancy icons for Dired
(use-package nerd-icons-dired
  :after (:all nerd-icons dired)
  :hook (dired-mode . nerd-icons-dired-mode))

;;; Diredfl
;; Colorful dired
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;;; WDired
;; Rename files in Dired
(use-package wdired
  :ensure nil
  :after (dired undo-tree)
  :hook (wdired-mode . turn-on-undo-tree-mode))

;;; Fd-Dired
;; `find-dired' alternative using `fd'
(use-package fd-dired
  :if (executable-find "fd"))

;;; _
(provide 'init-file-management)
