;; init-file-management.el --- -*- lexical-binding: t; -*-

;; TODO Checkout some of the Dired extension packages e.g. Dired+

;;; Dired
;; Editing not just files but file meta
(use-package dired
  :ensure nil
  :after (evil general)
  :hook ((dired-mode . lgreen/setup-dired-mode-map)
         (dired-mode . (lambda () (dired-omit-mode 1))))
  :custom
  (delete-by-moving-to-trash t)
  (dired-dwim-target t)
  (dired-auto-revert-buffer t)
  (dired-omit-files "^\\.[^.].*")
  :init
  (defun lgreen/setup-dired-mode-map ()
    ;; NOTE: The `dired-mode-map' is setup in a strange way such that
    ;; we need to add the local leader prefix `SPC m' using `evil-define-key'
    ;; and we have to put the `dired-narrow' binding here instead of in
    ;; the `dired-narrow' `use-package' block.
    (evil-define-key 'normal dired-mode-map (kbd "SPC") nil)
    (evil-define-key 'normal dired-mode-map (kbd "<RET>") 'dired-find-alternate-file)
    (evil-define-key 'normal dired-mode-map (kbd "<TAB>") 'dired-subtree-toggle)
    (evil-define-key 'normal dired-mode-map (kbd "SPC m") (make-sparse-keymap))
    (evil-define-key 'normal dired-mode-map (kbd "SPC m /") 'dired-narrow)
    (evil-define-key 'normal dired-mode-map (kbd "SPC m t o") 'dired-omit-mode)
    (evil-define-key 'normal dired-mode-map (kbd "SPC m t c") 'dired-collapse-mode))
  )

;;; Dired-Narrow
;; Find the files on the straight-&-narrow
(use-package dired-narrow
  :after (general dired))

;;; Nerd-Icons-Dired
;; Fancy icons for Dired
(use-package nerd-icons-dired
  :after (:all nerd-icons dired)
  :hook (dired-mode . nerd-icons-dired-mode))

;;; Diredfl
;; Colorful Dired
(use-package diredfl
  :after dired
  :hook (dired-mode . diredfl-mode))

;;; Dired-Subtree
;; Inline sub-tree expansion
(use-package dired-subtree
  :after dired)

;;; Dired-Collapse
;; Flatten the emptiness
(use-package dired-collapse
  :after dired)

;;; WDired
;; Rename files in Dired
(use-package wdired
  :ensure nil
  :after (dired undo-tree)
  :hook (wdired-mode . turn-on-undo-tree-mode))

;;; Fd-Dired
;; `find-dired' alternative using `fd'
(use-package fd-dired
  :when (executable-find "fd"))

;;; _
(provide 'init-file-management)
