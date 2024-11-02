;; init-snippets.el --- -*- lexical-binding: t; -*-

;;; Yasnippet
;; Template power!
(use-package yasnippet
  :commands yas-minor-mode
  :hook (((prog-mode text-mode) . yas-minor-mode)
         (yas-minor-mode . (lambda () (yas-activate-extra-mode 'fundamental-mode))))
  :init
;;;; Keymaps
  (lgreen/leader-define-key
    :keymaps 'yas-minor-mode-map
    "i" '(:ignore t :wk "Insert")
    "i i" '(yas-insert-snippet :wk "insert snippet")
    "i n" '(yas-new-snippet :wk "new snippet")
    "i v" '(yas-visit-snippet-file :wk "visit snippet file"))
;;;; _
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
