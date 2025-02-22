;; init.el --- -*- lexical-binding: t; -*-

;;; ======================
;;; Lambert's Emacs Config
;;; ======================
;;; Load Path
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;;; Requires
(require 'init-package-manager)
(require 'init-emacs)
(require 'init-keybindings)
(require 'init-evil)
(require 'init-editor)
(require 'init-undo)
(require 'init-minibuffer)
(require 'init-minibuffer-completion)
(require 'init-window-management)
(require 'init-os)
(require 'init-themes)
(require 'init-ui)
(require 'init-dashboard)
(require 'init-projects)
(require 'init-workspaces)
(require 'init-version-control)
(require 'init-file-management)
(require 'init-file-tree)
(require 'init-terminal)
(require 'init-completion)
(require 'init-spellchecking)
(require 'init-folding)
(require 'init-snippets)
(require 'init-org)
(require 'init-prog-modes)
(require 'init-cpp)
(require 'init-python)
(require 'init-elisp-mode)
(require 'init-my-elisp-functions)
(require 'init-utils)
(require 'init-squint)
(require 'init-tty)
(require 'init-email)
(require 'init-ai-utils)

;;; _
