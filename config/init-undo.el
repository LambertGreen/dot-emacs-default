;;; init-undo.el --- -*- lexical-binding: t; -*-

;;; Undo-Tree
;; "What good is a mind if you can't change it"
(use-package undo-tree
  :after (evil no-littering)
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-enable-undo-in-region t)
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-history-directory-alist
   `((".*" . ,(expand-file-name "undo-tree-hist/" no-littering-var-directory))))
  :hook
  (evil-local-mode . turn-on-undo-tree-mode)
  :general
  (:states 'normal
	   "U" 'undo-tree-visualize)
  :bind (:map undo-tree-map
	   ("C-/" . nil))
  :config
  (global-undo-tree-mode))
;;; _
(provide 'init-undo)
