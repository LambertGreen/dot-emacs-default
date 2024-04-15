;;; init-evil.el --- -*- lexical-binding: t; -*-


;;; Evil
;; The root of all money-trees
(use-package evil
  :custom
  ;; we set the below since we also using evil-collection
  ;; and it will take care of enabling evil keybindings
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  (evil-want-C-u-delete t)
  (evil-want-Y-yank-to-eol t)
  (evil-undo-system 'undo-tree)
  :config
  ;; Use evil-define-key to set keybindings in insert mode for C-a and C-e
  (evil-define-key 'insert 'global (kbd "C-a") 'move-beginning-of-line)
  (evil-define-key 'insert 'global (kbd "C-e") 'move-end-of-line)

  ;; Set keybinding for evil-avy-goto-char-timer in motion state
  (evil-define-key '(normal visual) 'global (kbd "g s SPC") 'evil-avy-goto-char-timer)

  ;; Activate evil-mode
  (evil-mode 1))

;;; Evil-Args
;; Argue your point left and right
(use-package evil-args
  :after evil
  :init
  ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

  (lgreen/local-leader-define-key
    :keymaps 'prog-mode-map
    "a" '(:ignore t :wk "Args")
    "a n" '(evil-forward-arg :wk "Forward arg")
    "a p" '(evil-backward-arg :wk "Backward arg")
    "a o" '(evil-jump-out-args :wk "Jump out arg")))

;;; Evil-Collection
;; keybind them all
(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-calendar-setup-org-bindings t)
  :config
  (evil-collection-init)
  ;; Unmap keys in 'evil-maps. If not done, (setq org-return-follows-link t) will not work
  ;; TODO Validate that you really need to unmap these keys
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "SPC") nil)
    (define-key evil-motion-state-map (kbd "RET") nil)
    (define-key evil-motion-state-map (kbd "TAB") nil)))

;;; Evil-Commentary
;; no pleading the fifth here
(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode))

;;; Evil EasyMotion
;; get there in one fell swoop
(use-package evil-easymotion
  :after evil
  :config (evilem-default-keybindings "gs"))

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

;;; Evil-Numbers
(use-package evil-numbers
  :after evil
  :config
  (general-define-key
   :states '(normal visual)
   :keymaps 'global
   "g =" 'evil-numbers/inc-at-pt
   "g -" 'evil-numbers/dec-at-pt))

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

;;; Evil-Textobj-Tree-Sitter
(use-package evil-textobj-tree-sitter
  :after evil-collection-unimpaired
  :config
  (evil-define-key nil evil-outer-text-objects-map
    "f" (evil-textobj-tree-sitter-get-textobj "function.outer")
    "l" (evil-textobj-tree-sitter-get-textobj "loop.outer")
    "x" (evil-textobj-tree-sitter-get-textobj "conditional.outer")
    "c" (evil-textobj-tree-sitter-get-textobj "class.outer")
    ;; "b" (evil-textobj-tree-sitter-get-textobj "block.outer")
    "a" (evil-textobj-tree-sitter-get-textobj "parameter.outer")
    )
  (evil-define-key nil evil-inner-text-objects-map
    "f" (evil-textobj-tree-sitter-get-textobj "function.inner")
    "l" (evil-textobj-tree-sitter-get-textobj "loop.inner")
    "x" (evil-textobj-tree-sitter-get-textobj "conditional.inner")
    "c" (evil-textobj-tree-sitter-get-textobj "class.inner")
    ;; "b" (evil-textobj-tree-sitter-get-textobj "block.inner")
    "a" (evil-textobj-tree-sitter-get-textobj "parameter.inner")
    )

    (evil-collection-define-key 'normal 'evil-collection-unimpaired-mode-map
      "]f" (lambda ()
	(interactive)
	(evil-textobj-tree-sitter-goto-textobj "function.outer"))
      "[f" (lambda ()
	(interactive)
	(evil-textobj-tree-sitter-goto-textobj "function.outer" t))
      "]F" (lambda ()
	(interactive)
	(evil-textobj-tree-sitter-goto-textobj "function.outer" nil t))
      "[F" (lambda ()
	(interactive)
	(evil-textobj-tree-sitter-goto-textobj "function.outer" t t))
      "]c" (lambda ()
	(interactive)
	(evil-textobj-tree-sitter-goto-textobj "class.outer"))
      "[c" (lambda ()
	(interactive)
	(evil-textobj-tree-sitter-goto-textobj "class.outer" t))
      "]C" (lambda ()
	(interactive)
	(evil-textobj-tree-sitter-goto-textobj "class.outer" nil t))
      "[C" (lambda ()
	(interactive)
	(evil-textobj-tree-sitter-goto-textobj "class.outer" t t))))

;;; Evil-Visualstar
;;  the star of the show
(use-package evil-visualstar
  :after evil)

;; ;;; Evil-Owl
;; TODO Evaluate evil-owl package
;; ;;  the star of the show
;; (use-package evil-owl
;;   :after evil)

;;; _
(provide 'init-evil)
