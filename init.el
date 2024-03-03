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
(require 'init-dired)
(require 'init-file-tree)
(require 'init-term)
(require 'init-elisp-mode)
(require 'init-my-elisp-functions)
(require 'init-dashboard)

;;; _
(provide 'init-evil)
