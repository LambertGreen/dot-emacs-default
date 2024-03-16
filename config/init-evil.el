;;; init-evil.el --- -*- lexical-binding: t; -*-


;;; Evil
;; The root of all money
(use-package evil
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

;;; Evil-Args
;; Argue your point left and right
(use-package evil-args
  :after evil
  :init
  ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

  (lgreen/local-leader-keys
   :keymaps 'prog-mode-map
   "a" '(:ignore t :wk "Args")
   "a n" '(evil-forward-arg :wk "Forward arg")
   "a p" '(evil-backward-arg :wk "Backward arg")
   "a o" '(evil-jump-out-args :wk "Jump out arg")))

;;; Evil-Collection
;; keybind them all
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  ;; Unmap keys in 'evil-maps. If not done, (setq org-return-follows-link t) will not work
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "SPC") nil)
    (define-key evil-motion-state-map (kbd "RET") nil)
    (define-key evil-motion-state-map (kbd "TAB") nil)))

;;; Evil-Commentary
;; no pleading the fifth here
(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode))

;;; Evil-Escape
;; alternate escape route when capslock is not bound to escape
(use-package evil-escape
  :after evil
  :custom
  (evil-escape-key-sequence "jk")
  (evil-escape-delay 0.2)
  :config
  (evil-escape-mode))

;;; Evil-Exchange
;; gx operator, like vim-exchange
;; Note: using cx like vim-exchange is possible but not as straightforward
(use-package evil-exchange
  :after evil
  :general
  (:states '(visual normal)
	   "gx" 'evil-exchange
	   "gX" 'evil-exchange-cancel))


;;; Evil-Expat
;; additional ex commands
(use-package evil-expat
  :after evil)

;;; Evil-Goggles
;; visual hints while editing
(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-use-diff-faces)
  (evil-goggles-mode))

;;; Evil-Indent-Plus
(use-package evil-indent-plus
  :after evil
  :config (evil-indent-plus-default-bindings))

;;; Evil-Lion
;; gl and gL operators, for lining things up
(use-package evil-lion
  :after evil
  :general
  (:states '(visual normal)
	   "gl" 'evil-lion-left
	   "gL" 'evil-lion-right))

;;; Evil EasyMotion
;; get there in one fell swoop
(use-package evil-easymotion
  :after evil
  :config (evilem-default-keybindings "gs"))

;;; Evil-Replace-With-Register
;; gR operator.  Replace without affecting unamed register
(use-package evil-replace-with-register
  :after evil
  :config (evil-replace-with-register-install))

;;; Evil-Snipe
(use-package evil-snipe
  :after evil
  :custom (evil-snipe-repeat-scope 'visible)
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1)
  :hook (magit-mode-hook . turn-off-evil-snipe-override-mode))

;;; Evil-Surround
;; encompass them on both sides
;; Keybindings provided by evil-surround:
;; cs<from><to>: Change Surrounding, changes surrounding <from> chars to <to>
;; ds<target>: Delete Surrounding, removes surrounding <target> chars
;; ys<target><to>: Add Surrounding, adds surrounding <to> chars around <target>
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;;; Evil-Visualstar
;;  the star of the show
(use-package evil-visualstar
  :after evil)

;;; _
(provide 'init-evil)
