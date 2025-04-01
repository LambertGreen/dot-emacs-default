;; init-evil.el --- -*- lexical-binding: t; -*-

;;; Evil
;; The root of all money-trees
(use-package evil
  :demand t
  :custom
  ;; we set the below since we also using evil-collection
  ;; and it will take care of enabling evil keybindings
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  (evil-want-C-u-delete t)
  (evil-want-C-i-jump t)
  (evil-want-Y-yank-to-eol t)
  (evil-symbol-word-search t)
  (evil-search-module 'evil-search)
  (evil-undo-system 'undo-tree)

  :config
;;;; Keymaps
;;;;; Insert mode
  ;; Use evil-define-key to set keybindings in insert mode for C-a and C-e
  (evil-define-key 'insert 'global (kbd "C-a") 'move-beginning-of-line)
  (evil-define-key 'insert 'global (kbd "C-e") 'move-end-of-line)
  (evil-define-key 'insert 'global (kbd "C-d") 'delete-char)

;;;;; Avy Goto
  ;; Set keybinding for evil-avy-goto-char-timer in motion state
  (evil-define-key '(normal visual) 'global
    (kbd "g s SPC") 'evil-avy-goto-char-timer       ;; Existing binding for timed char search
    (kbd "g s k")   'evil-avy-goto-line-above       ;; Go to line above
    (kbd "g s j")   'evil-avy-goto-line-below       ;; Go to line below
    (kbd "g s w")   'evil-avy-goto-word-0           ;; Go to the start of word
    (kbd "g s e")   'evil-avy-goto-word-1           ;; Go to any position in word
    (kbd "g s l")   'evil-avy-goto-line)            ;; Go to any line

  ;; Activate evil-mode
  (evil-mode 1))

;;; Evil-Args
;; Argue your point left and right
(use-package evil-args
  :after evil
  :init

;;;; Keymaps
;;;;; Text-Object Args
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
  :demand t
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-calendar-setup-org-bindings t)
  :config
  (evil-collection-init)

;;;; Keymaps
;;;;; Unbind SPC and RET
  (with-eval-after-load 'evil-maps
    ;; Unbind RET in both Normal and Motion states as
    ;; the default is to enter newline, and we don't need
    ;; buffer changes in normal mode
    (define-key evil-normal-state-map (kbd "RET") nil)
    (define-key evil-motion-state-map (kbd "RET") nil)

    (define-key evil-motion-state-map (kbd "SPC") nil)))

;;; Evil-Commentary
;; no pleading the fifth here
(use-package evil-commentary
  :after evil
  :init (evil-commentary-mode))

;;; Evil EasyMotion
;; get there in one fell swoop
;; TODO Do we need evil-easymotion or is our config of Avy good enough
(use-package evil-easymotion
  :after evil
  :disabled t
  :commands (evilem-default-keybindings)
  :config (evilem-default-keybindings "gs"))

;;; Evil-Escape
;; alternate escape route when capslock is not bound to escape
;; NOTE: disabling because I often spam home row keys, and I am also OK resorting to <C-[>
(use-package evil-escape
  :disabled t
  :after evil
  :custom
  (evil-escape-key-sequence "jk")
  (evil-escape-delay 0.2)
  (evil-escape-excluded-major-modes '(dired-mode treemacs-mode evil-visual-mode))
  :config
  (evil-escape-mode))

;;; Evil-Exchange
;; gx operator, like vim-exchange
;; Note: using cx like vim-exchange is possible but not as straightforward
(use-package evil-exchange
  :after evil
  :commands (evil-exchange)
  :general
;;;; Keymaps
;;;;; Use gx/X
  (:states '(visual normal)
           "gx" 'evil-exchange
           "gX" 'evil-exchange-cancel))

;;; Evil-Expat
;; additional ex commands
;; TODO Remove evil-expat if not missed now that it is disabled
(use-package evil-expat
  :disabled t
  :after evil)

;;; Evil-Goggles
;; visual hints while editing
(use-package evil-goggles
  :after evil
  :demand t
  :config
  (evil-goggles-use-diff-faces)
  (evil-goggles-mode))

;;; Evil-Indent-Plus
;; text-object on indentation
(use-package evil-indent-plus
  :after evil
  :demand t
  :config (evil-indent-plus-default-bindings))

;;; Evil-Lion
;; gl and gL operators, for lining things up
(use-package evil-lion
  :after evil
  :commands (evil-lion-left evil-lion-right)
  :general
;;;; Keymaps
;;;;; Use gl/L
  (:states '(visual normal)
           "gl" 'evil-lion-left
           "gL" 'evil-lion-right))

;;; Evil-Numbers
;; Up and down we go
;; Try on this number: 2
(use-package evil-numbers
  :after (general evil)
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt)
  :init
;;;; Keymaps
;;;;; Use g(=/-)
  (general-define-key
   :states '(normal visual)
   :keymaps 'global
   "g =" 'evil-numbers/inc-at-pt
   "g -" 'evil-numbers/dec-at-pt))

;;; Evil-Replace-With-Register
;; gR operator.  Replace without affecting unnamed register
(use-package evil-replace-with-register
  :after evil
  :commands (evil-replace-with-register-install)
  :init (evil-replace-with-register-install))

;;; Evil-Snipe
;; Putting f/F/t/T in the scope of s/S
(use-package evil-snipe
  :after evil
  :commands (evil-snipe-s evil-snipe-S)
  :custom
  (evil-snipe-repeat-scope 'visible)
  (evil-snipe-override-mode +1)
  :config
  (evil-snipe-mode +1)
  :hook (magit-mode-hook . turn-off-evil-snipe-override-mode))

;;; Evil-Surround
;; encompass them on both sides
;; Keybindings provided by evil-surround:
;; cs<from><to>: Change Surrounding, changes surrounding <from> chars to <to>
;; ds<target>: Delete Surrounding, removes surrounding <target> chars
;; ys<target><to>: Add Surrounding, adds surrounding <to> chars around <target>
(use-package evil-surround
  :after evil
  :commands global-evil-surround-mode
  :init
  (global-evil-surround-mode 1))

;;; Evil-Textobj-Line
;; operate on line items
(use-package evil-textobj-line
  :after evil
  :commands (evil-inner-line evil-a-line)
  :general
  (:keymaps 'evil-inner-text-objects-map
            "l" 'evil-inner-line)
  (:keymaps 'evil-outer-text-objects-map
            "l" 'evil-a-line))

;;; Evil-Textobj-Tree-Sitter
;; FIXME Enable for prog modes that have treesitter support
(use-package evil-textobj-tree-sitter
  :disabled t
  :after evil-collection-unimpaired
  :config
  (evil-define-key nil evil-outer-text-objects-map
    "f" (evil-textobj-tree-sitter-get-textobj "function.outer")
    "l" (evil-textobj-tree-sitter-get-textobj "loop.outer")
    "x" (evil-textobj-tree-sitter-get-textobj "conditional.outer")
    "c" (evil-textobj-tree-sitter-get-textobj "class.outer")
    ;; "b" (evil-textobj-tree-sitter-get-textobj "block.outer")
    "a" (evil-textobj-tree-sitter-get-textobj "parameter.outer"))
  (evil-define-key nil evil-inner-text-objects-map
    "f" (evil-textobj-tree-sitter-get-textobj "function.inner")
    "l" (evil-textobj-tree-sitter-get-textobj "loop.inner")
    "x" (evil-textobj-tree-sitter-get-textobj "conditional.inner")
    "c" (evil-textobj-tree-sitter-get-textobj "class.inner")
    ;; "b" (evil-textobj-tree-sitter-get-textobj "block.inner")
    "a" (evil-textobj-tree-sitter-get-textobj "parameter.inner"))
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
;; the star of the show
;; Keys: */#
(use-package evil-visualstar
  :after evil
  :commands (evil-visualstar/begin-search-forward evil-visualstar/begin-search-backward)
  :config
  (global-evil-visualstar-mode 1))

;;; Evil-Little-Word
;; handle the sub ("little") words in AnyCamelCase
(use-package evil-little-word
  :ensure (:fetcher github :repo "tarao/evil-plugins" :files ("evil-little-word.el"))
  :after evil
  :commands (evil-forward-little-word-begin evil-backward-little-word-begin evil-inner-little-word)
  :init
;;;; Keymaps
;;;;; Use M-(w/b)
  (define-key evil-normal-state-map (kbd "M-w") 'evil-forward-little-word-begin)
  (define-key evil-normal-state-map (kbd "M-b") 'evil-backward-little-word-begin)
  (define-key evil-operator-state-map (kbd "M-w") 'evil-forward-little-word-begin)
  (define-key evil-operator-state-map (kbd "M-b") 'evil-backward-little-word-begin)
  (define-key evil-visual-state-map (kbd "M-w") 'evil-forward-little-word-begin)
  (define-key evil-visual-state-map (kbd "M-b") 'evil-backward-little-word-begin)
  (define-key evil-visual-state-map (kbd "i M-w") 'evil-inner-little-word))

;;; Evil-Markdown
;; Get down with it, get down with it
(use-package evil-markdown
  :after (evil markdown-mode)
  :ensure (:fetcher github :repo "Somelauw/evil-markdown")
  :commands (evil-markdown-set-key-theme)
  :init
;;;; Keymaps
;;;;; Use C-(j/k)
  (evil-define-key 'normal evil-markdown-mode-map
    (kbd "C-j") 'markdown-next-visible-heading
    (kbd "C-k") 'markdown-previous-visible-heading)
  :config
  (evil-markdown-set-key-theme '(textobjects navigation additional)))

;;; Evil-Owl
;;  "Mark my words! You register?"
(use-package evil-owl
  :after evil
  :commands evil-owl-mode
  :init (evil-owl-mode))

;;; Targets
;; Aiming at that there
(use-package targets
  :ensure (:fetcher github :repo "dvzubarev/targets.el")
  :commands (targets-setup targets-define-composite-to targets-define-to)
  :custom
  (targets-text-objects nil)
  :init
  (targets-setup nil)
  :config
  (targets-define-composite-to any-block
    (("(" ")" pair)
     ("[" "]" pair)
     ("{" "}" pair)
     ("<" ">" pair))
    :bind t
    :next-key "N"
    :last-key "L"
    :around-key nil
    :inside-key nil
    :keys "b")
  (targets-define-composite-to any-quote
    (("\"" "\"" quote) ("'" "'" quote))
    :bind t
    :next-key "N"
    :last-key "L"
    :around-key nil
    :inside-key nil
    :keys "q")
  (targets-define-to double-quote
                     ("\"" nil quote)
                     :bind t
                     :next-key "N"
                     :last-key "L"
                     :around-key nil
                     :inside-key nil
                     :keys "q"))

;;; Expand-Region
;; just like the universe is doing
(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region )))

;;; Evil Clever Parens
;; Moving with top form
;; TODO Decide if this package is worth using for elisp mode
;; NOTE The package is put in disabled state because it was impacting
;; unimpaired bindings i.e. those beginning with "[" and "]"
(use-package evil-cleverparens
  :ensure t
  :disabled t
  :after (evil smartparens)
  :hook ((emacs-lisp-mode lisp-mode) . evil-cleverparens-mode)
  :config
  (evil-define-key 'normal evil-cleverparens-mode-map
    (kbd "]m") #'evil-cp-end-of-defun
    (kbd "[m") #'evil-cp-beginning-of-defun))

;;; _
(provide 'init-evil)
