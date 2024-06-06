;;; init-prog-modes.el --- -*- lexical-binding: t; -*-

;;; Lisp functions
(use-package emacs
  :ensure nil
  :config
  (lgreen/leader-define-key
    "c f" '(lgreen/format-buffer :wk "format buffer"))

  (lgreen/local-leader-define-key
    :keymaps 'prog-mode-map
    "f" '(:ignore t :wk "format")
    "f b" '(lgreen/format-buffer :wk "format buffer")
    "x" '(:ignore t :wk "errors")
    "x l" '(consult-flymake :wk "list errors")
    "x p" '(flymake-goto-prev-error :wk "error previous")
    "x n" '(flymake-goto-next-error :wk "error next"))

  (defun lgreen/format-buffer ()
    "Format buffer with eglot or apheleia."
    (interactive)
    (if (bound-and-true-p eglot--managed-mode)
	(eglot-format-buffer)
      (call-interactively #'apheleia-format-buffer))))

;;; Treesit-Auto
;; Get all the syntax
;; FIXME Install the appropriate version of Treesitter with expected ABI, and re-enable
(use-package treesit-auto
  :disabled t
  :custom
  (treesit-auto-install 'prompt)
  (treesit-font-lock-level 4)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; Apheleia
;; Format code with minimal disruption
(use-package apheleia
  :config (apheleia-global-mode +1))

;;; Eglot-Booster
;; Boohoo
(use-package eglot-booster
  :ensure (:fetcher github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

;;; Cmake
;; Let's make them cpp projects
(use-package cmake-mode
  :mode
  (("CMakeLists\\.txt\\'" . cmake-mode)
   ("\\.cmake\\'"         . cmake-mode)))

;;; Dockerfile
;; Contain your excitement
(use-package dockerfile-mode
  :mode "Dockerfile\\'")

;;; SSH config
;; Silently connecting the dots
(use-package ssh-config-mode
  :mode
  ((".ssh/config\\'"       . ssh-config-mode)
   ("sshd?_config\\'"      . ssh-config-mode)
   ("known_hosts\\'"       . ssh-known-hosts-mode)
   ("authorized_keys\\'"   . ssh-authorized-keys-mode)))

;;; Yaml
;; Configs everywhere
(use-package yaml-mode)

;;; Markdown
;; Baby org
(use-package markdown-mode)

;;; Groovy
;; Get into the groove boy
(use-package groovy-mode)

;;; Ahk-Mode
(use-package ahk-mode)
;;; _
(provide 'init-prog-modes)
