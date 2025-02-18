;; init-undo.el --- -*- lexical-binding: t; -*-

;;; Undo-Tree
;; "What good is a mind if you can't change it"
(use-package undo-tree
  :demand t
  :after (evil no-littering)
  :custom
  ;; Disabling for now because it is a pretty noisy option
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist
   `((".*" . ,(expand-file-name "undo-tree-hist/" no-littering-var-directory))))
  ;; (undo-tree-enable-undo-in-region t)
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  :general
;;;; Keymaps
  (:states 'normal
           "U" 'undo-tree-visualize)
  :bind (:map undo-tree-map
              ("C-/" . nil))
  :init
;;;; Functions
  ;; Suppress undo-tree save messages
  (defun lgreen/undo-tree-suppress-save-message (orig-fun &rest args)
    (let ((inhibit-message t))  ;; Temporarily disable messages
      (apply orig-fun args)))
  (advice-add 'undo-tree-save-history :around #'lgreen/undo-tree-suppress-save-message)
  (global-undo-tree-mode))

;;; _
(provide 'init-undo)