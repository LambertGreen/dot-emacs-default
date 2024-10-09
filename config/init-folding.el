;; init-folding.el --- -*- lexical-binding: t; -*-


;;; Treesit-fold
(use-package treesit-fold
  :ensure (:fetcher github :repo "emacs-tree-sitter/treesit-fold")
  :after outline
  :hook (prog-mode . treesit-fold-mode)
  :config
  (setq evil-fold-list '(((treesit-fold-mode)
			  :open-all treesit-fold-open-all
			  :close-all treesit-fold-close-all
			  :toggle treesit-fold-toggle
			  :open treesit-fold-open
			  :close treesit-fold-close))))

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
;; Allows manual creation/deletion of free-form folds
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

;;; Hideshow (hs-minor-mode) for Emacs Lisp
(use-package hideshow
  :ensure nil
  :hook ((emacs-lisp-mode . hs-minor-mode)
	 (lisp-interaction-mode . hs-minor-mode))
  :config
  (add-to-list 'evil-fold-list
	       '((hs-minor-mode)
		 :open-all hs-show-all
		 :close-all hs-hide-all
		 :toggle hs-toggle-hiding
		 :open hs-show-block
		 :close hs-hide-block)
	       'append))

;; _
(provide 'init-folding)

;; Local Variables:
;; jinx-local-words: "emacs treesit"
;; End:
