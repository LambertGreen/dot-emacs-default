;; init-prog-modes.el --- -*- lexical-binding: t; -*-


;;; Lisp functions
(use-package emacs
  :ensure nil
  :init
  (lgreen/leader-define-key
    "c f" '(lgreen/format-buffer :wk "format buffer"))

  (lgreen/local-leader-define-key
    :keymaps 'prog-mode-map
    "f" '(:ignore t :wk "Format")
    "f b" '(lgreen/format-buffer :wk "format buffer")

    "x" '(:ignore t :wk "Errors")
    "x l" '(consult-flymake :wk "list errors")
    "x p" '(flymake-goto-prev-error :wk "error previous")
    "x n" '(flymake-goto-next-error :wk "error next")

    "n" '(:ignore t :wk "Narrow")
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
      (call-interactively #'apheleia-format-buffer))
    (untabify (point-min) (point-max)))

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

  ;; BUG Apheleia being enabled globally results in it auto-formatting files we are simply reading!
  ;; We need to read up on how to enable formatting while editing files e.g. only format changed sections
  ;; (apheleia-global-mode +1)
  )

;;; Aggressive-Indent-Mode
;; Do it now!
(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

;;; Dtrt-Indent
;; Guess file indentation
;; TODO How does fit in with `aggresive-indent' package?
(use-package dtrt-indent
  :config (dtrt-indent-mode 1))

;;; Whitespace Cleanup
;; TODO Validate this package indeed works as advertised
(use-package whitespace-cleanup-mode
  :hook (prog-mode . whitespace-cleanup-mode))

;;; Whitespace-Mode
;; Getting red in the face for the trailing space
(use-package whitespace
  :ensure nil
  :after display-fill-column-indicator
  :custom (whitespace-style '(face lines tabs trailing))
  (whitespace-line-column fill-column)
  :hook (prog-mode . whitespace-mode)
  :init
  (lgreen/leader-define-key
    "x w" '(:ignore t :which-key "whitespace")
    "x w t" '(whitespace-toggle-options :which-key "Whitespace Toggle Options")
    "x w r" '(whitespace-report :which-key "Whitespace Report")))

;;; Eglot
;; Emacs Polyglot LSP client
(use-package eglot
  :ensure nil
  :config
  (add-to-list 'eglot-server-programs '((python-ts-mode python-mode) . ("pyright-langserver" "--stdio"))))

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
;;;; AutoHotkey
;; Make Emacs a babel-fish
(use-package ahk-mode
  :config
  (defun lgreen/ahk-comment-block-dwim (arg)
    "Comment or uncomment current line or region using block notation.
     For details, see `comment-dwim'."
    (interactive "*P")
    (require 'newcomment)
    (ahk-comment-dwim arg)))

;;;; Apple Script
(use-package apples-mode)

;;;; Bash Automated Testing System
(use-package bats-mode)

;;;; Character Separated Values
(use-package csv-mode)

;;;; Fennel language
(use-package fennel-mode)

;;;; Generate Ninja (GN)
(use-package gn-mode)

;;;; Git
(use-package git-modes
  :mode (("\\.gitconfig\\'" . gitconfig-mode)
         ("\\.gitignore\\'" . gitignore-mode)))

;;;; Groovy
(use-package groovy-mode)

;;;; Jq scripts
(use-package jq-mode
  :after org
  :mode ("\\.jq\\'" . jq-mode)
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((jq . t))))

;;;; Justfiles
(use-package just-mode)

;;;; Lua
(use-package lua-mode
  :custom (lua-indent-level 3))

;;;; Markdown
(use-package markdown-mode
  :after general
  :init
  (lgreen/local-leader-define-key
    :keymaps 'markdown-mode-map
    "i" '(:keymap markdown-mode-style-map :wk "insert")
    "c" '(:keymap markdown-mode-command-map :wk "command"))
  :hook (markdown-mode . outline-minor-mode))

;;;; Mermaid
(use-package mermaid-mode)

;;;; Nix
(use-package nix-mode)

;;;; RFC
(use-package rfc-mode)

;;;; Shell scripts
(use-package sh-mode
  :ensure nil
  :custom
  (sh-basic-offset 4)
  (sh-indentation 4)
  :hook (sh-mode . hs-minor-mode))

;;;; STrace output
(use-package strace-mode)

;;;; TOML configs
(use-package toml-mode)

;;;; Vim scripts
(use-package vimrc-mode)

;;;; YAML configs
(use-package yaml-mode
  :mode ("\\.(yaml|yml)\\'" . yaml-mode))

;;;; PowerShell scripts
(use-package powershell)

;;;; Kotlin
(use-package kotlin-mode)

;; _
(provide 'init-prog-modes)

;; Local Variables:
;; jinx-local-words: "CMakeLists Dockerfile Treesitter apheleia cmake defun eglot emacs gitconfig gitignore jenv jq prog sshd stylua txt uncomment yaml yml"
;; End:
