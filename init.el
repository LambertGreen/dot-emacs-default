;; My personal emacs 'default' config.

(setq user-full-name "Lambert Green"
      user-mail-address "lambert.green@gmail.com")

;; Visuals
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(menu-bar-mode -1)          ; Disable the menu bar
(set-fringe-mode 10)        ; Give some breathing room

;; Theme
(load-theme 'wombat)

;; Font
(set-face-attribute 'default nil :font "Iosevka Nerd Font" :height 200)

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package evil
  :ensure t
  :defer .1 ;; don't block emacs when starting, load evil immediately after startup
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration nil) ;; required by evil-collection
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t) ;; like vim's 'splitright'
  (setq evil-split-window-below t) ;; like vim's 'splitbelow'
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode))

;; vim-like keybindings everywhere in emacs
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; gl and gL operators, like vim-lion
(use-package evil-lion
  :ensure t
  :bind (:map evil-normal-state-map
         ("g l " . evil-lion-left)
         ("g L " . evil-lion-right)
         :map evil-visual-state-map
         ("g l " . evil-lion-left)
         ("g L " . evil-lion-right)))

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
(use-package evil-expat
  :ensure t
  :defer t)

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

;; Add Doom's modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package counsel
  :ensure t)

(use-package ivy
  :diminish
  :config
  (ivy-mode 1))

(use-package swiper
  :ensure t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish
  )

(use-package google-this
  :config
  (google-this-mode 1))

(use-package magit
  :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(magit dired-narrow google-this evil-surround evil-goggles evil-expat evil-visualstar evil-replace-with-register evil-exchange evil-commentary evil-lion evil-collection which-key rainbow-delimiters swiper doom-modeline ivy use-package evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))

(use-package dired-narrow)
