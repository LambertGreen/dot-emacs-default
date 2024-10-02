;; init-prog-modes.el --- -*- lexical-binding: t; -*-


;;; Lisp functions
(use-package emacs
  :ensure nil
  :init
  (lgreen/leader-define-key
    "c f" '(lgreen/format-buffer :wk "format buffer"))

  (lgreen/local-leader-define-key
    :keymaps 'prog-mode-map
    "f" '(:ignore t :wk "format")
    "f b" '(lgreen/format-buffer :wk "format buffer")

    "x" '(:ignore t :wk "errors")
    "x l" '(consult-flymake :wk "list errors")
    "x p" '(flymake-goto-prev-error :wk "error previous")
    "x n" '(flymake-goto-next-error :wk "error next")

    "n" '(:ignore t :wk "narrow")
    "n r" '(narrow-to-region :wk "narrow to region")
    "n d" '(narrow-to-defun :wk "narrow to defun")
    "n p" '(narrow-to-page :wk "narrow to page")
    "n w" '(widen :wk "widen"))

  (defun lgreen/set-faces-for-prog-mode (&rest _)
    "Set faces for programming font lock variables"
    (interactive)

    ;; Make keywords italic and light weight
    (set-face-attribute 'font-lock-keyword-face nil :slant 'italic :weight 'light)
    ;; Make comments italic and light weight
    (set-face-attribute 'font-lock-comment-face nil :slant 'italic :weight 'light)
    ;; Make function names more prominent by increasing size
    ;; TODO Rather use a relative increase in font size
    (set-face-attribute 'font-lock-function-name-face nil :weight 'normal :height 148)
    )

  (defun lgreen/format-buffer ()
    "Format buffer with eglot or apheleia."
    (interactive)
    (if (bound-and-true-p eglot--managed-mode)
	(eglot-format-buffer)
      (call-interactively #'apheleia-format-buffer)))

  (defun lgreen/set-java-home-from-jenv ()
    "Set JAVA_HOME environment variable from jenv."
    (interactive)
    (let ((jenv-java-home (shell-command-to-string "jenv prefix")))
      (when (not (string= jenv-java-home ""))
	(setenv "JAVA_HOME" (replace-regexp-in-string "\n+$" "" jenv-java-home)))))

  :hook (prog-mode . lgreen/set-faces-for-prog-mode)
  :config
  (advice-add 'load-theme
	      :after 'lgreen/set-faces-for-prog-mode))

;;; Treesit-Auto
;; Get all the syntax
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  (treesit-font-lock-level 4)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; Apheleia
;; Format code with minimal disruption
(use-package apheleia
  :config
  ;; Remove the existing 'stylua' entry and replace it `-s' usage to perform a recursive search for the `sytlua.toml'
  ;; file
  (setf (alist-get 'stylua apheleia-formatters)
        '("stylua" "-s" "-"))

  (apheleia-global-mode +1))

;;; Aggressive-Indent-Mode
;; Do it now!
(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

;;; Eglot-Booster
;; Boohoo
(use-package eglot-booster
  :ensure (:fetcher github :repo "jdtsmith/eglot-booster")
  :after eglot
  :init
  (add-to-list 'exec-path "~/dev/pub/emacs-lsp-booster/target/release")
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

;;; Language modes
;; Make Emacs a babel-fish
(use-package ahk-mode
  :config
  (defun lgreen/ahk-comment-block-dwim (arg)
    "Comment or uncomment current line or region using block notation.
     For details, see `comment-dwim'."
    (interactive "*P")
    (require 'newcomment)
    (ahk-comment-dwim arg)))

(use-package apples-mode)
(use-package bats-mode)
(use-package csv-mode)
(use-package fennel-mode)
(use-package gn-mode)
(use-package git-modes
  :mode (("\\.gitconfig\\'" . gitconfig-mode)
	 ("\\.gitignore\\'" . gitignore-mode)))

(use-package groovy-mode)

(use-package jq-mode
  :after org
  :mode ("\\.jq\\'" . jq-mode)
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((jq . t))))

(use-package just-mode)

(use-package lua-mode
  :custom (lua-indent-level 3))

(use-package markdown-mode
  :after general
  :init
  (lgreen/local-leader-define-key
    :keymaps 'markdown-mode-map
    "i" '(:ignore t :wk "insert")
    "ii" '(markdown-insert-italic :wk "italic")
    "ib" '(markdown-insert-bold :wk "bold")
    "ih" '(markdown-insert-hr :wk "h-line")
    "il" '(markdown-insert-link :wk "link")
    "ic" '(markdown-insert-code :wk "code")
    "iq" '(markdown-insert-blockquote :wk "blockquote")
    "it" '(markdown-insert-table :wk "table")

    "c" '(:ignore t :wk "command")
    "cm" '(markdown-other-window :wk "other-window")
    "cp" '(markdown-preview :wk "preview")
    "ce" '(markdown-export :wk "export"))
  :hook (markdown-mode . outline-minor-mode))

(use-package mermaid-mode)
(use-package nix-mode)
(use-package rfc-mode)

(use-package sh-mode
  :ensure nil
  :hook (sh-mode . hs-minor-mode))

(use-package strace-mode)
(use-package toml-mode)
(use-package vimrc-mode)
(use-package yaml-mode
  :mode ("\\.(yaml|yml)\\'" . yaml-mode))

(use-package powershell)

;; _
(provide 'init-prog-modes)

;; Local Variables:
;; jinx-local-words: "CMakeLists Dockerfile Treesitter apheleia ce cmake cp defun eglot emacs gitconfig gitignore ib ih jdtsmith jq lsp prog sshd txt uncomment yaml yml"
;; End:
