;;; init.el --- -*- lexical-binding: t; -*-

;; My personal emacs 'default' config.

;;; Code:

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
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; Set personal info
(setq user-full-name "Lambert Green"
      user-mail-address "lambert.green@gmail.com")

;; Visuals
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(menu-bar-mode -1)          ; Disable the menu bar
(set-fringe-mode 10)        ; Give some breathing room

;; Font
(set-face-attribute 'default nil :font "Iosevka Nerd Font" :height 120)

;; Get a beautiful and functional theme
(use-package modus-themes
  :ensure
  :init
  (modus-themes-load-themes)
  :config
  (modus-themes-load-operandi)
  :bind ("<f5>" . modus-themes-toggle))

;; macOS: Change dark/light theme
(if (eq system-type 'darwin)
    (add-hook 'ns-system-appearance-change-functions
        #'(lambda (appearance)
                (mapc #'disable-theme custom-enabled-themes)
                (pcase appearance
                        ('light (modus-themes-load-operandi))
                        ('dark (modus-themes-load-vivendi))))))

;; Dired - tell it to use 'gls
(setq ls-lisp-use-insert-directory-program t)
(setq insert-directory-program "gls")

;;; Packages

;; Set a useful $PATH
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode))

;; vim-like keybindings everywhere in emacs
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; gl and gL operators, like vim-lion
(use-package evil-lion
  :bind (:map evil-normal-state-map
         ("gl " . evil-lion-left)
         ("gL " . evil-lion-right)
         :map evil-visual-state-map
         ("gl " . evil-lion-left)
         ("gL " . evil-lion-right)))

;; gc operator, like vim-commentary
(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode))

;; gx operator, like vim-exchange
;; NOTE using cx like vim-exchange is possible but not as straightforward
(use-package evil-exchange
  :bind (:map evil-normal-state-map
         ("gx" . evil-exchange)
         ("gX" . evil-exchange-cancel)))

;; gr operator, like vim's ReplaceWithRegister
(use-package evil-replace-with-register
  :bind (:map evil-normal-state-map
         ("gr" . evil-replace-with-register)
         :map evil-visual-state-map
         ("gr" . evil-replace-with-register)))

;; * operator in visual mode
(use-package evil-visualstar
  :bind (:map evil-visual-state-map
         ("*" . evil-visualstar/begin-search-forward)
         ("#" . evil-visualstar/begin-search-backward)))

;; ex commands, which a vim user is likely to be familiar with
(use-package evil-expat)

;; visual hints while editing
(use-package evil-goggles
  :config
  (evil-goggles-use-diff-faces)
  (evil-goggles-mode))

;; ;; like vim-surround
;; (use-package evil-surround
;;   :ensure t
;;   :commands
;;   (evil-surround-edit
;;    evil-Surround-edit
;;    evil-surround-region
;;    evil-Surround-region)
;;   :init
;;   (evil-define-key 'operator global-map "s" 'evil-surround-edit)
;;   (evil-define-key 'operator global-map "S" 'evil-Surround-edit)
;;   (evil-define-key 'visual global-map "S" 'evil-surround-region)
;;   (evil-define-key 'visual global-map "gS" 'evil-Surround-region))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Add Doom's modeline
;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1)
;;   :custom ((doom-modeline-height 15)))

;; Let's try this other modeline shall we?
(use-package telephone-line
  :init (telephone-line-mode 1))

(use-package counsel
  :ensure t)

(use-package ivy
  :diminish
  :config
  (ivy-mode 1))

(use-package swiper
  :ensure t)

(use-package which-key
  :init (which-key-mode)
  :diminish)

;; (use-package google-this
;;   :config
;;   (google-this-mode 1))

(use-package magit)

;; (use-package dired-narrow)

;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))
