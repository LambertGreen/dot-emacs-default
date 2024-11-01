;; init-prog-modes.el --- -*- lexical-binding: t; -*-


;;; Prog Mode
;; Enter the Matrix
(use-package prog
  :ensure nil
  :hook (prog-mode . lgreen/set-faces-for-prog-mode)
  :init
;;;; Keymaps
;;;;; Navigation
  (general-define-key
   :keymaps '(prog-mode-map)
   "M-n" (lambda () (interactive) (forward-evil-defun 1))
   "M-p" (lambda () (interactive) (forward-evil-defun -1)))

;;;;; Formatting
  (lgreen/leader-define-key
    "c f" '(lgreen/format-buffer :wk "format buffer"))
  (lgreen/local-leader-define-key
    :keymaps 'prog-mode-map
    "f" '(:ignore t :wk "Format")
    "f b" '(lgreen/format-buffer :wk "format buffer")

;;;;; Errors
    "x" '(:ignore t :wk "Errors")
    "x l" '(consult-flymake :wk "list errors")
    "x p" '(flymake-goto-prev-error :wk "error previous")
    "x n" '(flymake-goto-next-error :wk "error next")

;;;;; Narrowing
    "n" '(:ignore t :wk "Narrow")
    "n r" '(narrow-to-region :wk "narrow to region")
    "n d" '(narrow-to-defun :wk "narrow to defun")
    "n p" '(narrow-to-page :wk "narrow to page")
    "n w" '(widen :wk "widen"))
;;;; Functions
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
  :config
;;;; Advice
  (advice-add 'load-theme
              :after 'lgreen/set-faces-for-prog-mode))

;;; Treesit-Auto
;; Fast climbing the syntax tree
(use-package treesit-auto
  :commands (global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt)
  (treesit-font-lock-level 4)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  ;; TODO Remove the below after testing
  ;; (setq treesit-auto-excluded-major-modes '(org-mode))
  (global-treesit-auto-mode))
;;; Formatting
;;;; Apheleia
;; Format code with minimal disruption
(use-package apheleia
  :commands (apheleia-format-buffer apheleia-mode)
  :hook (emacs-lisp-mode . apheleia-mode)
  :config
  ;; Remove the existing 'stylua' entry and replace it `-s' usage to perform a recursive search for the `sytlua.toml'
  ;; file
  (setf (alist-get 'stylua apheleia-formatters)
        '("stylua" "-s" "-")))

;;; Indentation
;;;; Aggressive-Indent-Mode
;; Actively keeping code correctly indented
(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

;;;; Dtrt-Indent
;; Guess indentation offset from existing indentation in current file
(use-package dtrt-indent
  :hook (prog-mode . dtrt-indent-mode)
  :init (require 'smie))

;;; Whitespace Handling
;;;; Whitespace Cleanup
;;Automatically cleanup whitespace on-save for files that were already compliant
(use-package whitespace-cleanup-mode
  :hook ((org-mode prog-mode) . whitespace-cleanup-mode))

;;;; Whitespace-Mode
;; Getting red in the face for the trailing space
(use-package whitespace
  :ensure nil
  :after display-fill-column-indicator
  :hook ((prog-mode . whitespace-prog-mode-setup)
         (org-mode . whitespace-org-mode-setup))
  :custom
  (whitespace-line-column fill-column)
  :init
;;;;; Keymaps
  (lgreen/leader-define-key
    "x w" '(:ignore t :which-key "whitespace")
    "x w t" '(whitespace-toggle-options :which-key "Whitespace Toggle Options")
    "x w r" '(whitespace-report :which-key "Whitespace Report"))
;;;;; Functions
  (defun whitespace-prog-mode-setup ()
    "Configure whitespace settings for prog-mode."
    (setq-local whitespace-style '(face lines tabs trailing))
    (whitespace-mode 1))

  (defun whitespace-org-mode-setup ()
    "Configure whitespace settings for org-mode."
    (setq-local whitespace-style '(face tabs trailing)) ;; No long line highlighting
    (whitespace-mode 1)))

;;; Language Server Protocol
;;;; Eglot
;; Emacs Polyglot LSP client
(use-package eglot
  :ensure nil
  :config
  (add-to-list 'eglot-server-programs '((python-ts-mode python-mode) . ("pyright-langserver" "--stdio"))))

;;;; Eglot-Booster
;; Making LSP usage bearable
(use-package eglot-booster
  :ensure (:fetcher github :repo "jdtsmith/eglot-booster")
  :after eglot
  :commands (eglot)
  :init
  (add-to-list 'exec-path "~/dev/pub/emacs-lsp-booster/target/release")
  :config (eglot-booster-mode))

;;; Language Modes
;;;; Cmake
;; Let's make them cpp projects
(use-package cmake-mode
  :mode
  (("CMakeLists\\.txt\\'" . cmake-mode)
   ("\\.cmake\\'"         . cmake-mode)))

;;;; Dockerfile
;; Putting in a container
(use-package dockerfile-mode
  :mode "Dockerfile\\'")

;;;; SSH config
;; Silently connecting the dots
(use-package ssh-config-mode
  :mode
  ((".ssh/config\\'"       . ssh-config-mode)
   ("sshd?_config\\'"      . ssh-config-mode)
   ("known_hosts\\'"       . ssh-known-hosts-mode)
   ("authorized_keys\\'"   . ssh-authorized-keys-mode)))

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
  :mode ("\\.jq\\'" . jq-mode)
  :init
  (with-eval-after-load 'org
    (add-to-list 'org-babel-load-languages '(jq . t))
    (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)))

;;;; Justfiles
(use-package just-mode)

;;;; Lua
(use-package lua-mode
  :custom (lua-indent-level 3))

;;;; Markdown
(use-package markdown-mode
  :after general
  :hook (markdown-mode . outline-minor-mode)
  :init
;;;;; Keymaps
  (lgreen/local-leader-define-key
    :keymaps 'markdown-mode-map
    "i" '(:keymap markdown-mode-style-map :wk "insert")
    "c" '(:keymap markdown-mode-command-map :wk "command")))

;;;; Mermaid
(use-package mermaid-mode)

;;;; Nix
(use-package nix-mode)

;;;; RFC
(use-package rfc-mode)

;;;; Shell scripts
(use-package sh-mode
  :ensure nil
  :hook (sh-mode . hs-minor-mode)
  :custom
  (sh-basic-offset 4)
  (sh-indentation 4))

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

;;; _
(provide 'init-prog-modes)

;; Local Variables:
;; jinx-local-words: "CMakeLists Dockerfile Treesitter apheleia cmake defun eglot emacs gitconfig gitignore jenv jq prog sshd stylua txt uncomment whitespace yaml yml"
;; End:
