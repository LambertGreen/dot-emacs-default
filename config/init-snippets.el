;; init-snippets.el --- -*- lexical-binding: t; -*-

;;; Yasnippet
;; Template power!
(use-package yasnippet
  :commands yas-minor-mode
  :hook ((prog-mode text-mode) . yas-minor-mode)
  :config
  (yas-reload-all))

;;; Yasnippet-Snippets
;; Collection of templates
(use-package yasnippet-snippets
  :after yasnippet)

;;; Lorem-Ipsum
(use-package lorem-ipsum)

;;; _
(provide 'init-snippets)
