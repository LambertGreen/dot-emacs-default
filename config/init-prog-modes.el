;; init-prog-modes.el --- -*- lexical-binding: t; -*-

;;; Prog Mode
;; Enter the Matrix
(use-package prog-mode
  :after general
  :ensure nil
  :hook ((prog-mode . lgreen/set-faces-for-prog-mode)
         (prog-mode . display-line-numbers-mode))
  :custom
  (display-line-numbers-type 'relative)
  :init
  ;; Workaround for https://github.com/noctuid/general.el/issues/193
  ;; :major-modes doesn't respect derived modes, so we must list them explicitly
  (defvar lgreen/prog-major-modes
    '(java-ts-mode
      c-ts-mode
      c++-ts-mode
      python-ts-mode
      rust-ts-mode
      emacs-lisp-mode
      ;; add others as needed
      )
    "Major modes that should receive prog-mode local leader bindings.")
;;;; Keymaps
;;;;; Navigation
  (general-define-key
   :keymaps '(prog-mode-map)
   "M-n" (lambda () (interactive) (forward-evil-defun 1))
   "M-p" (lambda () (interactive) (forward-evil-defun -1))
   "C-j" (lambda () (interactive) (forward-evil-defun 1))
   "C-k" (lambda () (interactive) (forward-evil-defun -1)))


;;;;; Formatting
  (lgreen/leader-define-key
    "c f" '(lgreen/format-buffer :wk "format buffer"))

  (lgreen/local-leader-define-key
    :keymaps 'prog-mode-map
    :major-modes lgreen/prog-major-modes

;;;;; GoTo/Navigation
    "g" '(:ignore t :wk "GoTo")
    "g d" '(lgreen/xref-find-definitions-with-fallback :wk "definition")
    "g D" '(xref-find-definitions-other-window :wk "definition other window")
    "g j" '(lgreen/xref-find-definitions-dumb-jump :wk "definition (force dumb-jump)")
    "g r" '(xref-find-references :wk "references")
    "g a" '(xref-find-apropos :wk "apropos (workspace symbols)")
    "g b" '(xref-go-back :wk "back")
    "g f" '(xref-go-forward :wk "forward")

;;;;; LSP
    "l" '(:ignore t :wk "LSP")
    "l a" '(lgreen/show-lsp-required-message :wk "actions")
    "l f" '(lgreen/show-lsp-required-message :wk "format")
    "l r" '(lgreen/show-lsp-required-message :wk "rename")

;;;;; Formatting
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
  (defun lgreen/xref-find-definitions-dumb-jump ()
    "Find definitions using dumb-jump backend via xref interface.
Forces dumb-jump even when LSP is active, useful as fallback when LSP fails."
    (interactive)
    (let ((xref-backend-functions '(dumb-jump-xref-activate)))
      (call-interactively #'xref-find-definitions)))

  (defun lgreen/xref-find-definitions-with-fallback ()
    "Find definitions with automatic fallback to dumb-jump if primary backend fails.
Tries the normal xref backend chain (LSP/eglot → treesit → dumb-jump).
If no definitions found, retries with only dumb-jump backend."
    (interactive)
    (condition-case err
        (call-interactively #'xref-find-definitions)
      (user-error
       ;; xref--not-found-error is a user-error, fall back to dumb-jump only
       (message "LSP failed, trying dumb-jump...")
       (let ((xref-backend-functions '(dumb-jump-xref-activate)))
         (call-interactively #'xref-find-definitions)))))

  (defun lgreen/set-faces-for-prog-mode (&rest _)
    "Set faces for programming font lock variables and function definitions."
    (interactive)
    ;; Make keywords italic and light weight
    (set-face-attribute 'font-lock-keyword-face nil :slant 'italic :weight 'light)
    ;; Make comments light weight
    (set-face-attribute 'font-lock-comment-face nil :slant 'italic :weight 'light)

    ;; Make function names slightly larger in tree-sitter modes
    (when (and (treesit-available-p)
               (derived-mode-p 'python-ts-mode 'c-ts-mode 'c++-ts-mode
                               'js-ts-mode 'tsx-ts-mode 'java-ts-mode
                               'rust-ts-mode))
      (set-face-attribute 'font-lock-function-name-face nil :height 1.1)))

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

  (defun lgreen/show-lsp-required-message ()
    "Indicates Action requires LSP"
    (interactive)
    (message "Action requires LSP. Start elgot with `M-x eglot' or enable in `.dir-locals.el'"))
  :config
;;;; Advice
  (advice-add 'load-theme :after 'lgreen/set-faces-for-prog-mode))

;;; Treesit-Auto
;; Fast climbing the syntax tree
(use-package treesit-auto
  :defer 0.1
  :custom
  (treesit-auto-install 'prompt)
  (treesit-font-lock-level 4)
  ;; Only enable tree-sitter for languages we actually use
  (treesit-auto-langs '(bash c cpp cmake css dockerfile go
                             groovy html java javascript json
                             kotlin lua nix python rust
                             toml tsx typescript yaml))
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
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
  (setf (alist-get 'stylua apheleia-formatters) '("stylua" "-s" "-")))


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
  (add-to-list 'eglot-server-programs '((python-ts-mode python-mode) . ("pyright-langserver" "--stdio")))
  (custom-set-faces
   '(eglot-semantic-modifier-face ((t (:inherit font-lock-keyword-face)))))
;;;;; Keymaps
  :general
  (lgreen/local-leader-define-key
    :keymaps 'eglot-mode-map
    "l a" '(eglot-code-actions :wk "actions (eglot)")
    "l f" '(eglot-format :wk "format (eglot)")
    "l r" '(eglot-rename :wk "rename (eglot)")))

;;;; Eglot-Booster
;; Making LSP usage bearable
(use-package eglot-booster
  :ensure (:host github :repo "jdtsmith/eglot-booster")
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

;;;; Conf mode
;; Make the key value pairs colorful
(use-package conf-mode
  :ensure nil
  :mode
  (("/\\.aws/credentials\\'" . conf-mode)
   ("/\\.aws/config\\'"      . conf-mode)))

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
(use-package just-mode
  :mode (("justfile\\'" . just-mode)
         ("\\.justfile\\'" . just-mode)))

;;;; Lua
(use-package lua-mode
  :custom (lua-indent-level 3))

;;;; Markdown
(use-package markdown-mode
  :after general
  :hook ((markdown-mode . outline-minor-mode)
         (markdown-mode . lgreen/markdown-font-setup))
  :custom
  ;; Hide markup like org-hide-emphasis-markers
  (markdown-hide-markup t)
  :init
;;;;; Keymaps
  (lgreen/local-leader-define-key
    :keymaps 'markdown-mode-map
    "i" '(:keymap markdown-mode-style-map :wk "insert")
    "c" '(:keymap markdown-mode-command-map :wk "command")
    "z" '(:ignore t :wk "Visibility")
    "z m" '(lgreen/markdown-toggle-markup :wk "toggle markup"))
  :config
;;;;; Visuals
  (defun lgreen/markdown-font-setup ()
    "Set up markdown faces to match org-mode styling."
    (interactive)
    (when (display-graphic-p)
      ;; Set heading heights to match org-mode
      (dolist (face '((markdown-header-face-1 . 1.3)
                      (markdown-header-face-2 . 1.20)
                      (markdown-header-face-3 . 1.17)
                      (markdown-header-face-4 . 1.15)
                      (markdown-header-face-5 . 1.1)
                      (markdown-header-face-6 . 1.1)))
        (set-face-attribute (car face) nil :weight 'medium :height (cdr face)))

      ;; Style code blocks similarly to org
      (set-face-attribute 'markdown-code-face nil :inherit 'fixed-pitch)
      (set-face-attribute 'markdown-inline-code-face nil :inherit '(shadow fixed-pitch))
      (set-face-attribute 'markdown-pre-face nil :inherit 'fixed-pitch)))

  (defun lgreen/markdown-toggle-markup ()
    "Toggle hiding/showing of markdown markup characters."
    (interactive)
    (setq markdown-hide-markup (not markdown-hide-markup))
    (markdown-reload-extensions)))

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

;;;; Feature Mode (Gherkin/Behave)
(use-package feature-mode
  :mode ("\\.feature\\'" . feature-mode))

;;; Compilation

;;;; Compile
;; Build and run programs
(use-package compile
  :ensure nil
  :hook (compilation-mode . goto-address-mode)
  :custom
  ;; Scroll compilation output to first error
  (compilation-scroll-output 'first-error))

;;; _
(provide 'init-prog-modes)
