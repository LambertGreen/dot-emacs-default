;;; init-evil.el --- -*- lexical-binding: t; -*-

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq org-return-follows-link t)
  :config
  ;; Use Emacs keybindings in insert mode for C-a and C-e
  (define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
  (evil-set-initial-state 'eat-mode 'insert)
  (evil-mode))

(use-package evil-args
  :ensure t
  :after evil
  :config
  ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

  (lgreen/local-leader-keys
   :keymaps 'prog-mode-map
   "a" '(:ignore t :wk "Args")
   "a n" '(evil-forward-arg :wk "Forward arg")
   "a p" '(evil-backward-arg :wk "Backward arg")
   "a o" '(evil-jump-out-args :wk "Jump out arg")))

;; vim-like keybindings everywhere in emacs
(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init)
  ;; Unmap keys in 'evil-maps. If not done, (setq org-return-follows-link t) will not work
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "SPC") nil)
    (define-key evil-motion-state-map (kbd "RET") nil)
    (define-key evil-motion-state-map (kbd "TAB") nil)))

;; gl and gL operators, like vim-lion
(use-package evil-lion
  :ensure t
  :bind (:map evil-normal-state-map
	      ("gl " . evil-lion-left)
	      ("gL " . evil-lion-right)
	      :map evil-visual-state-map
	      ("gl " . evil-lion-left)
	      ("gL " . evil-lion-right)))

;; gc operator, like vim-commentary
(use-package evil-commentary
  :ensure t
  :after evil
  :config (evil-commentary-mode))

;; gx operator, like vim-exchange
;; NOTE using cx like vim-exchange is possible but not as straightforward
(use-package evil-exchange
  :ensure t
  :bind (:map evil-normal-state-map
	      ("gx" . evil-exchange)
	      ("gX" . evil-exchange-cancel)))

;; gr operator, like vim's ReplaceWithRegister
(use-package evil-replace-with-register
  :ensure t
  :bind (:map evil-normal-state-map
	      ("gr" . evil-replace-with-register)
	      :map evil-visual-state-map
	      ("gr" . evil-replace-with-register)))

;; * operator in visual mode
(use-package evil-visualstar
  :ensure t
  :bind (:map evil-visual-state-map
	      ("*" . evil-visualstar/begin-search-forward)
	      ("#" . evil-visualstar/begin-search-backward)))

;; ex commands, which a vim user is likely to be familiar with
(use-package evil-expat :ensure t)

;; visual hints while editing
(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-use-diff-faces)
  (evil-goggles-mode))

;; like vim-surround
(use-package evil-surround
  :ensure t
  :commands
  (evil-surround-edit
   evil-Surround-edit
   evil-surround-region
   evil-Surround-region)
  :init
  (evil-define-key 'operator global-map "s" 'evil-surround-edit)
  (evil-define-key 'operator global-map "S" 'evil-Surround-edit)
  (evil-define-key 'visual global-map "S" 'evil-surround-region)
  (evil-define-key 'visual global-map "gS" 'evil-Surround-region))

;;; _
(provide 'init-evil)
