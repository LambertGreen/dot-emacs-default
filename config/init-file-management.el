;; init-file-management.el --- -*- lexical-binding: t; -*-

;; TODO Checkout some of the Dired extension packages e.g. Dired+

;;; Dired
;; Editing not just files but file meta
(use-package dired
  :ensure nil
  :after (evil general)
  :commands (dired dired-jump)
  :hook ((dired-mode . lgreen/setup-dired-mode-map)
         (dired-mode . dired-hide-details-mode)
         (dired-mode . (lambda () (dired-omit-mode 1)))
         (dired-mode . (lambda () (toggle-truncate-lines))))
  :custom
  (delete-by-moving-to-trash t)
  (dired-dwim-target t)
  (dired-auto-revert-buffer t)
  (dired-listing-switches "-lhtA --group-directories-first")
  (ls-lisp-dirs-first t)
  (dired-omit-files "^\\.[^.].*")
  :init
;;;; Keymaps
  (defun lgreen/setup-dired-mode-map ()
    ;; NOTE: The `dired-mode-map' is setup in a strange way such that
    ;; we need to add the local leader prefix `SPC m' using `evil-define-key'
    (general-def
      :keymaps 'dired-mode-map
      :states 'normal
      "SPC" nil
      "." 'dired-omit-mode
      "<RET>" 'dired-find-alternate-file
      "<TAB>" 'dired-subtree
      "C-k" 'dired-up-directory)

    (evil-define-key 'normal dired-mode-map (kbd "SPC m") (make-sparse-keymap)))
  )

;;; Dired-Narrow
;; Find the files on the straight-&-narrow
(use-package dired-narrow
  :after dired
  :init
;;;; Keymaps
  (defun lgreen/setup-dired-narrow-keys ()
    (lgreen/local-leader-define-key
      :keymaps 'dired-mode-map
      "n" '(:ignore t :wk "Narrow")
      "n n" '(dired-narrow :wk "narrow")
      "n f" '(dired-narrow-fuzzy :wk "fuzzy")
      "n r" '(dired-narrow-regexp :wk "regexp")))

  ;; NOTE: Using dired-mode-hook does not work due to dired delayed loading
  (advice-add 'lgreen/setup-dired-mode-map :after #'lgreen/setup-dired-narrow-keys)
  )

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
  :after dired
  :init
;;;; Keymaps
  (defun lgreen/setup-dired-subtree-keys ()
    (general-def
      :keymaps 'dired-mode-map
      :states 'normal
      "<TAB>" 'dired-subtree-toggle))
  ;; NOTE: Using dired-mode-hook does not work due to dired delayed loading
  (advice-add 'lgreen/setup-dired-mode-map :after #'lgreen/setup-dired-subtree-keys)
  )


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
