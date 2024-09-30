;; init-folding.el --- -*- lexical-binding: t; -*-

;;; Outline
(use-package outline
  :ensure nil
  :init
  (setq evil-fold-list '(((outline-mode outline-minor-mode)
                          :open-all outline-show-all
			  :close-all (lambda () (outline-hide-sublevels 1))  ;; Fix: pass a level argument
                          :toggle outline-toggle-children
                          :open show-entry
                          :close hide-subtree)))
  )

;;; Org
(use-package org
  :ensure nil
  :config
  (add-to-list 'evil-fold-list
               '((org-mode)
		 :open-all org-show-all
		 :close-all org-overview
		 :toggle org-cycle
		 :open org-show-entry
		 :close org-hide-entry
		 :open-rec org-show-subtree
		 :close-rec org-hide-subtree)
               'append)
  )

;;; Treesit-fold
(use-package treesit-fold
  :ensure (:fetcher github :repo "emacs-tree-sitter/treesit-fold")
  :after outline
  :hook (prog-mode . treesit-fold-mode)
  :config
  (add-to-list 'evil-fold-list
               '((treesit-fold-mode)
                 :open-all treesit-fold-open-all
                 :close-all treesit-fold-close-all
                 :toggle treesit-fold-toggle
                 :open treesit-fold-open
                 :close treesit-fold-close)
	       'append))

;;; Origami
;; Enable Vim like code folding
(use-package origami
  :hook (prog-mode . origami-mode)
  :after (outline treesit-fold)
  :config
  (add-to-list 'evil-fold-list
               '((origami-mode)
                 :open-all origami-open-all-nodes
                 :close-all origami-close-all-nodes
                 :toggle origami-toggle-node
                 :open origami-open-node
                 :close origami-close-node)
	       'append))

;;; Evil-Vimish-Fold
;; Allows manual creation/deletion of freeform folds
(use-package evil-vimish-fold
  :after outline
  :config
  (add-to-list 'evil-fold-list
               '((vimish-fold-mode)
                 :open-all vimish-fold-unfold-all
                 :close-all vimish-fold-refold-all
                 :toggle vimish-fold-toggle
                 :open vimish-fold-unfold
                 :close vimish-fold-refold)
               'append))

;; _
(provide 'init-folding)
