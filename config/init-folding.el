;; init-folding.el --- -*- lexical-binding: t; -*-


;;; Treesit-fold
(use-package treesit-fold
  :ensure (:fetcher github :repo "emacs-tree-sitter/treesit-fold")
  :after evil
  :hook (prog-mode . treesit-fold-mode)
  :init
  (setq evil-fold-list '(((treesit-fold-mode)
                          :open-all treesit-fold-open-all
                          :close-all treesit-fold-close-all
                          :toggle treesit-fold-toggle
                          :open treesit-fold-open
                          :close treesit-fold-close))))

;;; Hideshow (hs-minor-mode) for Emacs Lisp
(use-package hideshow
  :ensure nil
  :after (evil treesit-fold)
  :hook ((emacs-lisp-mode . hs-minor-mode)
         (lisp-interaction-mode . hs-minor-mode))
  :init
  (add-to-list 'evil-fold-list
               '((hs-minor-mode)
                 :open-all hs-show-all
                 :close-all hs-hide-all
                 :toggle hs-toggle-hiding
                 :open hs-show-block
                 :close hs-hide-block)
               'append))

;;; Origami and Evil Integration
;; Enable Vim-like code folding
(use-package origami
  :hook (prog-mode . origami-mode)
  :after (evil treesit-fold hideshow)
  :init
  ;; Define interactive functions for Origami folding operations
  (defun lgreen/origami-open-all-nodes ()
    "Open all folds in the current buffer using Origami."
    (interactive)
    (origami-open-all-nodes (current-buffer)))
  (defun lgreen/origami-close-all-nodes ()
    "Close all folds in the current buffer using Origami."
    (interactive)
    (origami-close-all-nodes (current-buffer)))
  (defun lgreen/origami-toggle-node ()
    "Toggle the fold at the current point using Origami."
    (interactive)
    (origami-toggle-node (current-buffer) (point)))
  (defun lgreen/origami-open-node ()
    "Open the fold at the current point using Origami."
    (interactive)
    (origami-open-node (current-buffer) (point)))
  (defun lgreen/origami-open-node-recursively ()
    "Recursively open the fold at the current point using Origami."
    (interactive)
    (origami-open-node-recursively (current-buffer) (point)))
  (defun lgreen/origami-close-node ()
    "Close the fold at the current point using Origami."
    (interactive)
    (origami-close-node (current-buffer) (point)))
  ;; Add Origami to evil-fold-list with the newly created functions
  (add-to-list 'evil-fold-list
               '((origami-mode)
                 :open-all   lgreen/origami-open-all-nodes
                 :close-all  lgreen/origami-close-all-nodes
                 :toggle     lgreen/origami-toggle-node
                 :open       lgreen/origami-open-node
                 :open-rec   lgreen/origami-open-node-recursively
                 :close      lgreen/origami-close-node)
               'append))

;;; Evil-Vimish-Fold
;; Allows manual creation/deletion of free-form folds
(use-package evil-vimish-fold
  :after evil
  :init
  (add-to-list 'evil-fold-list
               '((vimish-fold-mode)
                 :open-all vimish-fold-unfold-all
                 :close-all vimish-fold-refold-all
                 :toggle vimish-fold-toggle
                 :open vimish-fold-unfold
                 :close vimish-fold-refold)
               'append))

;;; _
(provide 'init-folding)

;; Local Variables:
;; jinx-local-words: "emacs treesit"
;; End:
