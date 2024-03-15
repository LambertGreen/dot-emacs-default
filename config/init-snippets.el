;;; init-snippets.el --- -*- lexical-binding: t; -*-

(use-package yasnippet-snippets
  :hook (prog-mode . yas-minor-mode))

(use-package lorem-ipsum)

;;; _
(provide 'init-snippets)
