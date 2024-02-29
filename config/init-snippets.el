;;; init-snippets.el --- -*- lexical-binding: t; -*-

(use-package yasnippet-snippets
  :ensure t
  :hook (prog-mode . yas-minor-mode))

(use-package lorem-ipsum
  :ensure t)

;;; _
(provide 'init-snippets)
