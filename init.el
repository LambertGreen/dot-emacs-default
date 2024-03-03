;;; init.el --- -*- lexical-binding: t; -*-


(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(require 'init-elpaca)
(require 'init-general)
(require 'init-evil)
(require 'init-emacs)
(require 'init-os)
(require 'init-ui)
(require 'init-completion)
(require 'init-folding)
(require 'init-snippets)
(require 'init-org)
(require 'init-projects)
(require 'init-vc)
(require 'init-file-tree)
(require 'init-term)
(require 'init-elisp-mode)
(require 'init-my-elisp-functions)

;; Evaluating below packages
;; -------------------------
;; (use-package magit-todos
;;   :ensure t
;;   :after magit
;;   :config (magit-todos-mode 1))

;; (use-package consult-todo
;;   :ensure t
;;   :after consult
;;   :commands (consult-todo consult-todo-all)

;; (use-package dired-narrow :ensure t)

;;; _
(provide 'init-evil)
