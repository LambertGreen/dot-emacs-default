;;; init.el --- -*- lexical-binding: t; -*-


(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(require 'init-package-manager)
(require 'init-keybindings)
(require 'init-evil)
(require 'init-emacs)
(require 'init-minibuffer)
(require 'init-minibuffer)
(require 'init-minibuffer-completion)
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
(require 'init-folding)
(require 'init-snippets)
(require 'init-org)
(require 'init-prog-modes)
(require 'init-elisp-mode)
(require 'init-my-elisp-functions)
(require 'init-cpp)

;;; _
(provide 'init-evil)
