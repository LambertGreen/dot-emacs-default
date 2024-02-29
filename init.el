;;; init.el --- -*- lexical-binding: t; -*-


(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(require 'init-elpaca)
(require 'init-general)
(require 'init-evil)
(require 'init-emacs)
(require 'init-ui)
(require 'init-completion)
(require 'init-folding)
(require 'init-snippets)
(require 'init-org)
(require 'init-projects)
(require 'init-vc)
(require 'init-term)

;; (use-package magit-todos
;;   :ensure t
;;   :after magit
;;   :config (magit-todos-mode 1))

;; (use-package consult-todo
;;   :ensure t
;;   :after consult
;;   :commands (consult-todo consult-todo-all)

(use-package emacs
  :config

  ;; Set transparency of emacs
  (defun lgreen/transparency (value)
    "Sets the transparency of the frame window. 0=transparent/100=opaque"
    (interactive "nTransparency Value 0 - 100 opaque:")
    (set-frame-parameter (selected-frame) 'alpha value)))

(use-package dired-narrow :ensure t)
