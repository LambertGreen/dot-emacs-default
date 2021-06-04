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
(set-face-attribute 'default nil :font "Fira Code Retina" :height 280)

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

;; Download Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Enable Evil
(require 'evil)
(evil-mode 1)
